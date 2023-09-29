use crate::capabilities::{negotiate_capabilities, NegotiatedCapabilities};
use crate::config::{Config, CONFIG_KEY};
use crate::{convert, handler, lsp_ext, UrlExt, Vfs, MAX_FILE_LEN};
use anyhow::{bail, Context, Result};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, ErrorCode, LanguageClient, ResponseError};
use gleam_interop;
use ide::{
    module_name, Analysis, AnalysisHost, Cancelled, Dependency, FileSet, ModuleMap, PackageGraph,
    PackageId, PackageInfo, PackageRoot, SourceRoot, Target, VfsPath,
};
use la_arena::Idx;
use lsp_types::notification::Notification;
use lsp_types::request::{self as req, Request};
use lsp_types::{
    notification as notif, ConfigurationItem, ConfigurationParams, DidChangeConfigurationParams,
    DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, FileChangeType, FileEvent, FileSystemWatcher, GlobPattern,
    InitializeParams, InitializeResult, InitializedParams, MessageType, NumberOrString, OneOf,
    ProgressParams, ProgressParamsValue, PublishDiagnosticsParams, Registration,
    RegistrationParams, RelativePattern, ServerInfo, ShowMessageParams, Url, WorkDoneProgress,
    WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    WorkDoneProgressReport,
};
use smol_str::SmolStr;
use tokio::sync::oneshot;

use std::backtrace::Backtrace;
use std::borrow::BorrowMut;
use std::cell::Cell;
use std::collections::HashMap;

use std::future::{ready, Future};
use std::io::{ErrorKind, Read};
use std::ops::ControlFlow;
use std::panic::UnwindSafe;
use std::path::{Path, PathBuf};

use std::sync::{Arc, Once, RwLock};
use std::time::Duration;
use std::{fmt, panic};
use tokio::task;
use tokio::task::{AbortHandle, JoinHandle};
use toml::Table;
use walkdir::WalkDir;

type NotifyResult = ControlFlow<async_lsp::Result<()>>;

struct UpdateConfigEvent(serde_json::Value);
struct SetPackageGraphEvent(Option<(PackageGraph, Vec<PackageRoot>)>);

const LSP_SERVER_NAME: &str = "gleamalyzer";
pub const GLEAM_TOML: &str = "gleam.toml";
pub const MANIFEST_TOML: &str = "manifest.toml";
const LOAD_WORKSPACE_PROGRESS_TOKEN: &str = "gleamalyzer/loadWorkspaceProgress";

const LOAD_GLEAM_WORKSPACE_DEBOUNCE_DURATION: Duration = Duration::from_millis(100);

pub struct Server {
    // States.
    /// This contains an internal RWLock and must not lock together with `vfs`.
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    opened_files: HashMap<Url, FileData>,
    config: Arc<Config>,

    // Ongoing tasks.
    load_gleam_workspace_fut: Option<JoinHandle<()>>,
    init_channel: Option<oneshot::Sender<()>>,

    client: ClientSocket,
    capabilities: NegotiatedCapabilities,
    /// Messages to show once initialized.
    init_messages: Vec<ShowMessageParams>,
}

#[derive(Debug, Default)]
struct FileData {
    diagnostics_task: Option<AbortHandle>,
}

impl Server {
    pub fn new_router(client: ClientSocket, init_messages: Vec<ShowMessageParams>) -> Router<Self> {
        let this = Self::new(client, init_messages);
        let mut router = Router::new(this);
        router
            //// Lifecycle ////
            .request::<req::Initialize, _>(Self::on_initialize)
            .notification::<notif::Initialized>(Self::on_initialized)
            .request::<req::Shutdown, _>(|_, _| ready(Ok(())))
            .notification::<notif::Exit>(|_, _| ControlFlow::Break(Ok(())))
            //// Notifications ////
            .notification::<notif::DidOpenTextDocument>(Self::on_did_open)
            .notification::<notif::DidCloseTextDocument>(Self::on_did_close)
            .notification::<notif::DidChangeTextDocument>(Self::on_did_change)
            .notification::<notif::DidChangeConfiguration>(Self::on_did_change_configuration)
            // NB. This handler is mandatory.
            // > In former implementations clients pushed file events without the server actively asking for it.
            // Ref: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
            .notification::<notif::DidChangeWatchedFiles>(Self::on_did_change_watched_files)
            //// Requests ////
            .request_snap::<req::GotoDefinition>(handler::goto_definition)
            .request_snap::<req::Completion>(handler::completion)
            .request_snap::<req::HoverRequest>(handler::hover)
            .request_snap::<lsp_ext::SyntaxTree>(handler::syntax_tree)
            //// Events ////
            .event(Self::on_set_package_info)
            .event(Self::on_update_config)
            // Loopback event.
            .event(Self::on_did_change_watched_files);
        router
    }

    pub fn new(client: ClientSocket, init_messages: Vec<ShowMessageParams>) -> Self {
        Self {
            host: AnalysisHost::default(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            opened_files: HashMap::default(),
            config: Arc::new(Config::new("/non-existing-path".into())),

            load_gleam_workspace_fut: None,
            init_channel: None,

            client,
            init_messages,
            capabilities: NegotiatedCapabilities::default(),
        }
    }

    fn on_initialize(
        &mut self,
        params: InitializeParams,
    ) -> impl Future<Output = Result<InitializeResult, ResponseError>> {
        tracing::info!("Init params: {params:?}");

        let (server_caps, final_caps) = negotiate_capabilities(&params);

        self.capabilities = final_caps;

        let (tx, rx) = oneshot::channel::<()>();
        self.init_channel = Some(tx);

        // TODO: Use `workspaceFolders`.
        let root_path = match params
            .root_uri
            .as_ref()
            .and_then(|uri| uri.to_file_path().ok())
        {
            Some(path) => path,
            None => std::env::current_dir().expect("Failed to the current directory"),
        };

        let mut cfg = Config::new(root_path.clone());
        if let Some(options) = params.initialization_options {
            let (errors, _updated_diagnostics) = cfg.update(options);
            if !errors.is_empty() {
                let msg = ["Failed to apply some settings:"]
                    .into_iter()
                    .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                    .collect::<String>();
                self.client.show_message_ext(MessageType::ERROR, msg);
            }
        }
        *Arc::get_mut(&mut self.config).expect("No concurrent access yet") = cfg;

        // Make a virtual event to trigger loading of gleam.toml files for package info.
        let gleam_toml_changed_event = DidChangeWatchedFilesParams {
            changes: [GLEAM_TOML]
                .into_iter()
                .map(|name| {
                    let uri = Url::from_file_path(self.config.root_path.join(name))
                        .expect("Root must be absolute");
                    let typ = FileChangeType::CREATED;
                    FileEvent { uri, typ }
                })
                .collect(),
        };

        self.on_did_change_watched_files(gleam_toml_changed_event);

        async {
            let _ = rx.await;
            Ok(InitializeResult {
                capabilities: server_caps,
                server_info: Some(ServerInfo {
                    name: LSP_SERVER_NAME.into(),
                    version: option_env!("CFG_RELEASE").map(Into::into),
                }),
            })
        }
    }

    fn on_initialized(&mut self, _params: InitializedParams) -> NotifyResult {
        for msg in std::mem::take(&mut self.init_messages) {
            tracing::warn!("Init message ({:?}): {}", msg.typ, msg.message);
            let _: Result<_, _> = self.client.show_message(msg);
        }

        // FIXME: This is still racy since `on_did_open` can also trigger reloading and would
        // read uninitialized configs.
        self.spawn_reload_config();

        ControlFlow::Continue(())
    }

    async fn register_watched_files(
        config: &Config,
        caps: &NegotiatedCapabilities,
        client: &mut ClientSocket,
    ) {
        let to_watcher = |pat: &str| FileSystemWatcher {
            glob_pattern: if caps.watch_files_relative_pattern {
                let root_uri = Url::from_file_path(&config.root_path).expect("Must be absolute");
                GlobPattern::Relative(RelativePattern {
                    base_uri: OneOf::Right(root_uri),
                    pattern: pat.into(),
                })
            } else {
                GlobPattern::String(format!("{}/{}", config.root_path.display(), pat))
            },
            // All events.
            kind: None,
        };
        let register_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: ["**/*.gleam", GLEAM_TOML].map(to_watcher).into(),
        };
        let params = RegistrationParams {
            registrations: vec![Registration {
                id: notif::DidChangeWatchedFiles::METHOD.into(),
                method: notif::DidChangeWatchedFiles::METHOD.into(),
                register_options: Some(serde_json::to_value(register_options).unwrap()),
            }],
        };
        if let Err(err) = client.register_capability(params).await {
            client.show_message_ext(
                MessageType::ERROR,
                format!("Failed to watch gleam.toml: {err:#}"),
            );
        }
        tracing::info!("Registered file watching for gleam.toml");
    }

    fn on_did_open(&mut self, params: DidOpenTextDocumentParams) -> NotifyResult {
        // Ignore the open event for unsupported files, thus all following interactions
        // will error due to unopened files.
        let len = params.text_document.text.len();
        if len > MAX_FILE_LEN {
            self.client.show_message_ext(
                MessageType::WARNING,
                "Disable LSP functionalities for too large file ({len} > {MAX_FILE_LEN})",
            );
            return ControlFlow::Continue(());
        }

        let uri = params.text_document.uri;
        self.opened_files.insert(uri.clone(), FileData::default());
        self.set_vfs_file_content(&uri, params.text_document.text);

        self.spawn_update_diagnostics(uri);

        ControlFlow::Continue(())
    }

    fn on_did_close(&mut self, params: DidCloseTextDocumentParams) -> NotifyResult {
        // N.B. Don't clear text here.
        // `DidCloseTextDocument` means the client ends its maintainance to a file but
        // not deletes it.
        self.opened_files.remove(&params.text_document.uri);

        // Clear diagnostics for closed files.
        let _: Result<_, _> =
            self.client
                .notify::<notif::PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: params.text_document.uri,
                    diagnostics: Vec::new(),
                    version: None,
                });

        ControlFlow::Continue(())
    }

    fn on_did_change(&mut self, params: DidChangeTextDocumentParams) -> NotifyResult {
        let mut vfs = self.vfs.write().unwrap();
        let uri = params.text_document.uri;
        // Ignore files not maintained in Vfs.
        let Ok(file) = vfs.file_for_uri(&uri) else { return ControlFlow::Continue(()) };
        for change in params.content_changes {
            let ret = (|| {
                let del_range = match change.range {
                    None => None,
                    Some(range) => Some(convert::from_range(&vfs, file, range).ok()?.1),
                };
                vfs.change_file_content(file, del_range, &change.text)
                    .ok()?;
                Some(())
            })();
            if ret.is_none() {
                tracing::error!(
                    "File is out of sync! Failed to apply change for {uri}: {change:?}"
                );

                // Clear file states to minimize pollution of the broken state.
                self.opened_files.remove(&uri);
                let _: Result<_, _> = vfs.remove_uri(&uri);
            }
        }
        drop(vfs);

        // FIXME: This blocks.
        self.apply_vfs_change();

        self.spawn_update_diagnostics(uri);

        ControlFlow::Continue(())
    }

    fn on_did_change_configuration(
        &mut self,
        _params: DidChangeConfigurationParams,
    ) -> NotifyResult {
        // As stated in https://github.com/microsoft/language-server-protocol/issues/676,
        // this notification's parameters should be ignored and the actual config queried separately.
        self.spawn_reload_config();
        ControlFlow::Continue(())
    }

    fn on_did_change_watched_files(&mut self, params: DidChangeWatchedFilesParams) -> NotifyResult {
        tracing::debug!("Watched files changed: {params:?}");

        let mut gleam_toml_changed = false;
        for &FileEvent { ref uri, mut typ } in &params.changes {
            // Don't reload files maintained by the client.
            if self.opened_files.contains_key(uri) {
                continue;
            }
            let Ok(path) = uri.to_file_path() else { continue };

            if matches!(typ, FileChangeType::CREATED | FileChangeType::CHANGED) {
                match (|| -> std::io::Result<_> {
                    #[cfg(unix)]
                    use rustix::fs::{fcntl_getfl, fcntl_setfl, OFlags, OpenOptionsExt};

                    // Rule out non-regular files which may block `open()` infinitely
                    // (eg. FIFO). We open it with `O_NONBLOCK` and check it before reading.
                    let mut options = std::fs::File::options();
                    options.read(true);
                    #[cfg(unix)]
                    options.custom_flags(OFlags::NONBLOCK.bits() as _);

                    let mut file = options.open(&path)?;
                    let ft = file.metadata()?.file_type();
                    if !ft.is_file() {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!("non-regular file type: {ft:?}"),
                        ));
                    }

                    // Remove the O_NONBLOCK flag for blocking read.
                    #[cfg(unix)]
                    {
                        let flags = fcntl_getfl(&file)? - OFlags::NONBLOCK;
                        fcntl_setfl(&file, flags)?;
                    }

                    let mut buf = String::new();
                    file.read_to_string(&mut buf)?;
                    Ok(buf)
                })() {
                    Ok(text) => self.set_vfs_file_content(uri, text),
                    Err(err) if matches!(err.kind(), ErrorKind::NotFound) => {
                        // File gets removed at the time calling `open()`.
                        typ = FileChangeType::DELETED;
                    }
                    Err(err) => tracing::error!("Ignore file {path:?}: {err}"),
                }
            }
            if typ == FileChangeType::DELETED {
                let _: Result<_> = self.vfs.write().unwrap().remove_uri(uri);
            }

            if let Ok(relative) = path.strip_prefix(&self.config.root_path) {
                if relative == Path::new(GLEAM_TOML) {
                    gleam_toml_changed = true;
                }
            }
        }

        if gleam_toml_changed {
            self.spawn_load_gleam_workspace();
        }

        ControlFlow::Continue(())
    }

    /// Spawn a task to (re)load the gleam workspace via `gleam.toml`
    fn spawn_load_gleam_workspace(&mut self) {
        let channel = std::mem::take(&mut self.init_channel);
        let fut = task::spawn(Self::load_gleam_workspace(
            self.vfs.clone(),
            self.config.clone(),
            self.capabilities.clone(),
            self.client.clone(),
            channel,
        ));
        if let Some(prev_fut) = self.load_gleam_workspace_fut.replace(fut) {
            prev_fut.abort();
        }
    }

    async fn load_gleam_workspace(
        vfs: Arc<RwLock<Vfs>>,
        config: Arc<Config>,
        caps: NegotiatedCapabilities,
        mut client: ClientSocket,
        channel: Option<oneshot::Sender<()>>,
    ) {
        // Delay the loading to debounce. Later triggers will cancel previous tasks.
        tokio::time::sleep(LOAD_GLEAM_WORKSPACE_DEBOUNCE_DURATION).await;

        tracing::info!("Loading gleam workspace");
        let progress = Progress::new(
            &client,
            &caps,
            LOAD_WORKSPACE_PROGRESS_TOKEN,
            "Loading workspace",
            "Downloading deps".to_owned(),
        )
        .await;
        match Self::load_packages(&vfs, &config).await {
            Ok(ret) => {
                let ret_info = ret
                    .as_ref()
                    .map(|r: &(PackageGraph, Vec<PackageRoot>)| r.0.clone());
                let _: Result<_, _> = client.emit(SetPackageGraphEvent(ret));
                if let Some(channel) = channel {
                    let _ = channel.send(());
                }
                ret_info
            }
            Err(err) => {
                client.show_message_ext(
                    MessageType::ERROR,
                    format!("Failed to load gleam workspace: {err:#}"),
                );
                return;
            }
        };
        progress.done(None);
    }

    async fn load_packages(
        vfs: &RwLock<Vfs>,
        config: &Config,
    ) -> Result<Option<(PackageGraph, Vec<PackageRoot>)>> {
        let mut graph = PackageGraph::default();
        tracing::info!("Downloading deps and loading package info");

        let _: () = gleam_interop::load_package_info()
            .await
            .context("Could not load dependencies")?;

        let mut package_roots = Vec::new();
        package_roots.push(PackageRoot {
            is_local: true,
            path: config.root_path.clone(),
        });
        let mut seen = HashMap::new();
        let _ = Self::assemble_graph(
            vfs,
            &config.root_path,
            &mut graph,
            &mut package_roots,
            &mut seen,
            true,
        );

        tracing::info!("GRAAAAPH {:#?} {:#?}", graph, package_roots);

        for package in &package_roots {
            Self::load_package_files(vfs, &package.path);
        }

        Ok(Some((graph, package_roots)))
    }

    fn assemble_graph(
        vfs: &RwLock<Vfs>,
        root_path: &PathBuf,
        graph: &mut PackageGraph,
        roots: &mut Vec<PackageRoot>,
        seen: &mut HashMap<SmolStr, PackageId>,
        is_local: bool,
    ) -> Result<PackageId> {

        roots.push(PackageRoot {
            path: root_path.to_path_buf(),
            is_local,
        });

        let (gleam_file, gleam_src) = {
            let mut vfs = vfs.write().unwrap();

            let gleam_path = root_path.join(GLEAM_TOML);
            let gleam_toml_src = std::fs::read_to_string(&gleam_path)?;
            let gleam_toml_vpath = VfsPath::new(gleam_path);
            let gleam_file = vfs.set_path_content(gleam_toml_vpath, gleam_toml_src);

            let src = vfs.content_for_file(gleam_file);
            (gleam_file, src)
        };
        tracing::info!("Assemblyyyy {:?} {:?}", root_path, gleam_src);
        let gleam_toml = gleam_src.parse::<Table>().unwrap();
        let name: SmolStr = gleam_toml
            .get("name")
            .and_then(|n| n.as_str())
            .context("No valid name")?
            .into();

        let direct_deps = gleam_toml
            .get("dependencies")
            .and_then(|d| d.as_table().cloned())
            .unwrap_or_default();

        let target: Target = gleam_toml
            .get("target")
            .and_then(|v| v.as_str())
            .unwrap_or_else(|| "erlang")
            .into();

        let package = graph.add_package(name, gleam_file);

        tracing::info!("Assemblyyyy graph {:?}", graph);
        let package_dir = root_path.join("build/packages");

        for (name, dep) in direct_deps.iter() {
            if let Some(dep_id) = seen.get(name.as_str()) {
                let dependency = Dependency {
                    package: dep_id.clone(),
                    name: name.to_string(),
                };
                graph.add_dep(package, dependency);
                continue;
            }
            let path = if is_local {
 
                package_dir.join(name)
            } else {
                root_path.parent().unwrap().join(name)
            };
            let dep_id = match dep
                .as_table()
                .and_then(|t| t.get("path"))
                .and_then(|v| v.as_str())
            {
                Some(local_path) => {
                    let local_path = root_path.join(local_path);
                    tracing::info!("LOCAL PATH{:?}", local_path);
                    let Ok(dep_id) = Self::assemble_graph(vfs, &local_path, graph, roots, seen, false) else {
                        continue;
                    };
                    dep_id
                },
                None => {
                    let Ok(dep_id) = Self::assemble_graph(vfs, &path, graph, roots, seen, false) else {
                        continue;
                    };
                    dep_id
                }
            };

            seen.insert(name.into(), dep_id.clone());

            let dependency = Dependency {
                package: dep_id,
                name: name.to_string(),
            };
            graph.add_dep(package, dependency);
        }
        // tracing::info!("Deps {:#?}", transitive_deps);

        graph.set_target(target);

        Ok(package)
    }

    fn load_package_files(vfs: &RwLock<Vfs>, root_path: &std::path::PathBuf) {
        let walkdir = WalkDir::new(root_path)
            .follow_links(true)
            .into_iter()
            .filter_entry(|entry| {
                if !entry.file_type().is_dir() {
                    return true;
                }
                tracing::info!("walking dir {:?}", entry.path());
                root_path == entry.path()
                    || ((entry.path().ends_with("src")) || entry.path().ends_with("test"))
                    || entry.depth() > 1
            });
        let files = walkdir.filter_map(|it| it.ok()).filter_map(|entry| {
            let is_file = entry.file_type().is_file();
            if !is_file {
                return None;
            }
            let entry_path = entry.into_path();
            if let Some(ext) = entry_path.extension() {
                if ext == "gleam" {
                    return Some(entry_path);
                }
            }
            None
        });
        let mut vfs = vfs.write().unwrap();
        for file in files {
            if let Some(src) = std::fs::read_to_string(file.clone()).ok() {
                vfs.set_path_content(VfsPath::from(file), src);
            }
        }
    }

    fn on_set_package_info(&mut self, info: SetPackageGraphEvent) -> NotifyResult {
        tracing::debug!("Set package info: {:#?}", info.0);
        let mut vfs = self.vfs.write().unwrap();

        vfs.set_package_graph(info.0.as_ref().map(|i| i.0.clone()));

        let roots = info.0.map(|i| i.1.clone()).unwrap_or_else(|| {
            vec![PackageRoot {
                path: self.config.root_path.clone(),
                is_local: true,
            }]
        });

        let (root_sets, module_map) = Self::lower_vfs(&mut vfs, &roots);
        vfs.set_roots_and_map(root_sets, module_map);

        drop(vfs);

        self.apply_vfs_change();

        // This is currently mostly to get proper diagnostics on startup
        let uris = self.opened_files.keys().cloned().collect::<Vec<_>>();
        for uri in uris {
            tracing::trace!("Recalculate diagnostics of {uri}");
            self.spawn_update_diagnostics(uri.clone());
        }
        ControlFlow::Continue(())
    }

    fn spawn_reload_config(&self) {
        if !self.capabilities.workspace_configuration {
            return;
        }
        let mut client = self.client.clone();
        tokio::spawn(async move {
            let ret = client
                .configuration(ConfigurationParams {
                    items: vec![ConfigurationItem {
                        scope_uri: None,
                        section: Some(CONFIG_KEY.into()),
                    }],
                })
                .await;
            let mut v = match ret {
                Ok(v) => v,
                Err(err) => {
                    client.show_message_ext(
                        MessageType::ERROR,
                        format_args!("Failed to update config: {err}"),
                    );
                    return;
                }
            };
            tracing::debug!("Updating config: {:?}", v);
            let v = v.pop().unwrap_or_default();
            let _: Result<_, _> = client.emit(UpdateConfigEvent(v));
        });
    }

    fn on_update_config(&mut self, value: UpdateConfigEvent) -> NotifyResult {
        let mut config = Config::clone(&self.config);
        let (errors, updated_diagnostics) = config.update(value.0);
        tracing::debug!("Updated config, errors: {errors:?}, config: {config:?}");
        self.config = Arc::new(config);

        if !errors.is_empty() {
            let msg = ["Failed to apply some settings:"]
                .into_iter()
                .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                .collect::<String>();
            self.client.show_message_ext(MessageType::ERROR, msg);
        }

        // Refresh all diagnostics since the filter may be changed.
        if updated_diagnostics {
            // Pre-collect to avoid mutability violation.
            let uris = self.opened_files.keys().cloned().collect::<Vec<_>>();
            for uri in uris {
                tracing::trace!("Recalculate diagnostics of {uri}");
                self.spawn_update_diagnostics(uri.clone());
            }
        }

        ControlFlow::Continue(())
    }

    fn spawn_update_diagnostics(&mut self, uri: Url) {
        let task = self.spawn_with_snapshot({
            let uri = uri.clone();
            move |snap| {
                // Return empty diagnostics for ignored files.
                (!snap.config.diagnostics_excluded_files.contains(&uri))
                    .then(|| {
                        with_catch_unwind("diagnostics", || handler::diagnostics(snap, &uri))
                            .unwrap_or_else(|err| {
                                tracing::error!("Failed to calculate diagnostics: {err}");
                                Vec::new()
                            })
                    })
                    .unwrap_or_default()
            }
        });

        // Can this really fail?
        let Some(f) = self.opened_files.get_mut(&uri) else { task.abort(); return; };
        if let Some(prev_task) = f.diagnostics_task.replace(task.abort_handle()) {
            prev_task.abort();
        }

        let mut client = self.client.clone();
        task::spawn(async move {
            if let Ok(diagnostics) = task.await {
                tracing::debug!("Publish {} diagnostics for {}", diagnostics.len(), uri);
                let _: Result<_, _> = client.publish_diagnostics(PublishDiagnosticsParams {
                    uri,
                    diagnostics,
                    version: None,
                });
            } else {
                // Task cancelled, then there must be another task queued already. Do nothing.
            }
        });
    }

    /// Create a blocking task with a database snapshot as the input.
    // NB. `spawn_blocking` must be called immediately after snapshotting, so that the read guard
    // held in `Analysis` is sent out of the async runtime worker. Otherwise, the read guard
    // is held by the async runtime, and the next `apply_change` acquiring the write guard would
    // deadlock.
    fn spawn_with_snapshot<T: Send + 'static>(
        &self,
        f: impl FnOnce(StateSnapshot) -> T + Send + 'static,
    ) -> JoinHandle<T> {
        let snap = StateSnapshot {
            analysis: self.host.snapshot(),
            vfs: Arc::clone(&self.vfs),
            config: Arc::clone(&self.config),
        };
        task::spawn_blocking(move || f(snap))
    }

    fn set_vfs_file_content(&mut self, uri: &Url, text: String) {
        let vpath = uri.to_vfs_path();
        self.vfs.write().unwrap().set_path_content(vpath, text);
        self.apply_vfs_change();
    }

    fn apply_vfs_change(&mut self) {
        let mut vfs = self.vfs.write().unwrap();

        let changes = vfs.take_change();
        drop(vfs);

        tracing::trace!("Apply VFS changes: {:?}", changes);
        // N.B. This acquires the internal write lock.
        // Must be called without holding the lock of `vfs`.
        self.host.apply_change(changes);
    }

    fn lower_vfs(
        vfs: &mut Vfs,
        source_root_config: &Vec<PackageRoot>,
    ) -> (Vec<SourceRoot>, ModuleMap) {
        let mut module_map = ModuleMap::default();

        let mut prefix_to_paths = HashMap::new();

        let mut prefix_components = Vec::new();

        for prefix in source_root_config {
            prefix_components.push(prefix.path.components().collect::<Vec<_>>())
        }

        //sorting by length matches the longest prefix first
        // e.g. /path/to/module is matched before /path
        prefix_components.sort_by(|a, b| b.len().cmp(&a.len()));

        for (file_id, file_path) in vfs.iter() {
            if let Some(path_components) = file_path.as_path() {
                let path_components: Vec<_> = path_components.components().collect();

                for prefix_components in &prefix_components {
                    if prefix_components.len() > path_components.len() {
                        continue;
                    }

                    let matching_prefix = prefix_components
                        .iter()
                        .zip(path_components.iter())
                        .all(|(prefix_comp, path_comp)| prefix_comp == path_comp);

                    if matching_prefix {
                        let prefix = prefix_components.iter().collect::<PathBuf>();

                        if let Some(module_name) = file_path
                            .as_path()
                            .and_then(|mpath| module_name(&prefix, mpath))
                        {
                            module_map.insert(file_id, module_name); // Todo: Report error if insert returns an fileid
                        }

                        prefix_to_paths
                            .entry(prefix)
                            .or_insert_with(FileSet::default)
                            .insert(file_id, file_path.to_owned());

                        break;
                    }
                }
            }
        }
        let source_roots = prefix_to_paths
            .into_iter()
            .enumerate()
            .map(|(i, (path, set))| {
                if i == 0 {
                    SourceRoot::new_local(set, path)
                } else {
                    SourceRoot::new_library(set, path)
                }
            })
            .collect();
        (source_roots, module_map)
    }
}

trait RouterExt: BorrowMut<Router<Server>> {
    fn request_snap<R: Request>(
        &mut self,
        f: impl Fn(StateSnapshot, R::Params) -> Result<R::Result> + Send + Copy + UnwindSafe + 'static,
    ) -> &mut Self
    where
        R::Params: Send + UnwindSafe + 'static,
        R::Result: Send + 'static,
    {
        self.borrow_mut().request::<R, _>(move |this, params| {
            let task = this.spawn_with_snapshot(move |snap| {
                with_catch_unwind(R::METHOD, move || f(snap, params))
            });
            async move {
                task.await
                    .expect("Already catch_unwind")
                    .map_err(error_to_response)
            }
        });
        self
    }
}

impl RouterExt for Router<Server> {}

trait ClientExt: BorrowMut<ClientSocket> {
    fn show_message_ext(&mut self, typ: MessageType, msg: impl fmt::Display) {
        // Maybe connect all tracing::* to LSP ShowMessage?
        let _: Result<_, _> = self.borrow_mut().show_message(ShowMessageParams {
            typ,
            message: msg.to_string(),
        });
    }
}

impl ClientExt for ClientSocket {}

struct Progress {
    client: ClientSocket,
    token: Option<String>,
}

impl Progress {
    async fn new(
        client: &ClientSocket,
        caps: &NegotiatedCapabilities,
        token: impl fmt::Display,
        title: impl fmt::Display,
        message: impl Into<Option<String>>,
    ) -> Self {
        let token = token.to_string();
        let created = caps.server_initiated_progress
            && client
                .request::<req::WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
                    token: NumberOrString::String(token.clone()),
                })
                .await
                .is_ok();
        let this = Self {
            client: client.clone(),
            token: created.then_some(token),
        };
        this.notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
            title: title.to_string(),
            cancellable: None,
            message: message.into(),
            percentage: None,
        }));
        this
    }

    fn notify(&self, progress: WorkDoneProgress) {
        let Some(token) = &self.token else { return };
        let _: Result<_, _> = self.client.notify::<notif::Progress>(ProgressParams {
            token: NumberOrString::String(token.clone()),
            value: ProgressParamsValue::WorkDone(progress),
        });
    }

    fn report(&self, percentage: u32, message: String) {
        assert!((0..=100).contains(&percentage));
        self.notify(WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message: Some(message),
            percentage: Some(percentage),
        }));
    }

    fn done(mut self, message: Option<String>) {
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message }));
        // Don't drop again.
        self.token = None;
    }
}

impl Drop for Progress {
    fn drop(&mut self) {
        self.notify(WorkDoneProgress::End(WorkDoneProgressEnd { message: None }));
    }
}

fn with_catch_unwind<T>(ctx: &str, f: impl FnOnce() -> Result<T> + UnwindSafe) -> Result<T> {
    static INSTALL_PANIC_HOOK: Once = Once::new();
    thread_local! {
        static PANIC_LOCATION: Cell<String> = Cell::new(String::new());
    }

    INSTALL_PANIC_HOOK.call_once(|| {
        let old_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            let loc = info
                .location()
                .map(|loc| loc.to_string())
                .unwrap_or_default();
            let backtrace = Backtrace::force_capture();
            PANIC_LOCATION.with(|inner| {
                inner.set(format!("Location: {loc:#}\nBacktrace: {backtrace:#}"));
            });
            old_hook(info);
        }));
    });

    match panic::catch_unwind(f) {
        Ok(ret) => ret,
        Err(payload) => {
            let reason = payload
                .downcast_ref::<String>()
                .map(|s| &**s)
                .or_else(|| payload.downcast_ref::<&str>().map(|s| &**s))
                .unwrap_or("unknown");
            let mut loc = PANIC_LOCATION.with(|inner| inner.take());
            if loc.is_empty() {
                loc = "Location: unknown".into();
            }
            tracing::error!("Panicked in {ctx}: {reason}\n{loc}");
            bail!("Panicked in {ctx}: {reason}\n{loc}");
        }
    }
}

fn error_to_response(err: anyhow::Error) -> ResponseError {
    if err.is::<Cancelled>() {
        return ResponseError::new(ErrorCode::REQUEST_CANCELLED, "Client cancelled");
    }
    match err.downcast::<ResponseError>() {
        Ok(resp) => resp,
        Err(err) => ResponseError::new(ErrorCode::INTERNAL_ERROR, err),
    }
}

#[derive(Debug)]
pub struct StateSnapshot {
    pub(crate) analysis: Analysis,
    vfs: Arc<RwLock<Vfs>>,
    pub(crate) config: Arc<Config>,
}

impl StateSnapshot {
    pub(crate) fn vfs(&self) -> impl std::ops::Deref<Target = Vfs> + '_ {
        self.vfs.read().unwrap()
    }
}
