use crate::capabilities::{negotiate_capabilities, NegotiatedCapabilities};
use crate::config::{Config, CONFIG_KEY};
use crate::{convert, handler, lsp_ext, UrlExt, Vfs, MAX_FILE_LEN};
use anyhow::{bail, ensure, Context, Result, Error};
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::tracing::TracingLayer;
use async_lsp::{
    ClientSocket, ErrorCode, LanguageClient, LanguageServer, ResponseError, ServerSocket,
};
use ide::{
    Analysis, AnalysisHost, Cancelled, Dependency, FileSet, PackageGraph, PackageId, PackageRoot,
    SourceRoot, Target, VfsPath,
};

use indexmap::IndexSet;
use lsp_types::notification::{Notification, PublishDiagnostics, ShowMessage};
use lsp_types::request::{self as req, Request};
use lsp_types::{
    notification as notif, ClientCapabilities, ConfigurationItem, ConfigurationParams,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams,
    DidChangeWatchedFilesRegistrationOptions, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams, FileChangeType,
    FileEvent, FileSystemWatcher, GlobPattern, InitializeParams, InitializeResult,
    InitializedParams, MessageType, NumberOrString, OneOf, Position, ProgressParams,
    ProgressParamsValue, PublishDiagnosticsParams, Range, Registration, RegistrationParams,
    RelativePattern, ServerInfo, ShowMessageParams, TextDocumentContentChangeEvent, TextEdit, Url,
    WindowClientCapabilities, WorkDoneProgress, WorkDoneProgressBegin,
    WorkDoneProgressCreateParams, WorkDoneProgressEnd, WorkDoneProgressReport,
};
use smol_str::SmolStr;
use tokio::io::AsyncWriteExt;

use tokio::sync::oneshot;
use tower::ServiceBuilder;

use std::backtrace::Backtrace;
use std::borrow::BorrowMut;
use std::cell::Cell;
use std::collections::HashMap;

use std::ffi::OsStr;
use std::future::{ready, Future};
use std::io::{ErrorKind, Read};
use std::ops::ControlFlow;
use std::panic::UnwindSafe;
use std::path::{Path, PathBuf};
use std::process::{self, Stdio};

use std::sync::{Arc, Once, RwLock};
use std::time::Duration;
use std::{fmt, panic};
use tokio::task;
use tokio::task::{AbortHandle, JoinHandle};
use toml::Table;
use walkdir::WalkDir;

type NotifyResult = ControlFlow<async_lsp::Result<()>>;

struct UpdateConfigEvent(serde_json::Value);
pub struct SetPackageGraphEvent(Option<(PackageGraph, Vec<PackageRoot>)>);

pub struct SettingState(pub oneshot::Receiver<()>);

#[derive(Debug)]
pub enum CollectDiagnosticsEvent {
    External(PublishDiagnosticsParams),
    Internal(PublishDiagnosticsParams),
}

#[derive(Debug)]
struct DiagnosticCollector {
    external: HashMap<Url, PublishDiagnosticsParams>,
    internal: HashMap<Url, PublishDiagnosticsParams>,
}

const LSP_SERVER_NAME: &str = "glas";
pub const GLEAM_TOML: &str = "gleam.toml";
pub const MANIFEST_TOML: &str = "manifest.toml";
const LOAD_WORKSPACE_PROGRESS_TOKEN: &str = "glas/loadWorkspaceProgress";

const LOAD_GLEAM_WORKSPACE_DEBOUNCE_DURATION: Duration = Duration::from_millis(100);

struct ClientState {}

pub struct Server {
    // States.
    /// This contains an internal RWLock and must not lock together with `vfs`.
    host: AnalysisHost,
    vfs: Arc<RwLock<Vfs>>,
    opened_files: HashMap<Url, FileData>,
    config: Arc<Config>,

    gleam_interop_client: Option<ServerSocket>,
    diagnostics: DiagnosticCollector,
    // Ongoing tasks.
    source_roots: IndexSet<PackageRoot>,

    client: ClientSocket,
    capabilities: NegotiatedCapabilities,
    /// Messages to show once initialized.
    init_messages: Vec<ShowMessageParams>,
}

struct InteropClient {
    client: ClientSocket,
}

impl InteropClient {
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::new(InteropClient { client });
        router
            .notification::<PublishDiagnostics>(Self::on_publish_notification)
            .notification::<notif::ShowMessage>(Self::on_notification);
        router
    }

    fn on_notification(&mut self, params: ShowMessageParams) -> NotifyResult {
        let _ = self.client.notify::<ShowMessage>(params);
        ControlFlow::Continue(())
    }

    fn on_publish_notification(&mut self, params: PublishDiagnosticsParams) -> NotifyResult {
        let _ = self
            .client
            .emit::<CollectDiagnosticsEvent>(CollectDiagnosticsEvent::External(params));
        ControlFlow::Continue(())
    }
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
            .request::<req::Formatting, _>(Self::formatting)
            .notification::<notif::Exit>(|_, _| ControlFlow::Break(Ok(())))
            //// Notifications ////
            .notification::<notif::DidOpenTextDocument>(Self::on_did_open)
            .notification::<notif::DidCloseTextDocument>(Self::on_did_close)
            .notification::<notif::DidChangeTextDocument>(Self::on_did_change)
            .notification::<notif::DidChangeConfiguration>(Self::on_did_change_configuration)
            .notification::<notif::DidSaveTextDocument>(Self::on_did_save)
            // NB. This handler is mandatory.
            // > In former implementations clients pushed file events without the server actively asking for it.
            // Ref: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
            .notification::<notif::DidChangeWatchedFiles>(Self::on_did_change_watched_files)
            //// Requests ////
            // .request_snap::<req::Formatting>(handler::formatting)
            .request_snap::<req::GotoDefinition>(handler::goto_definition)
            .request_snap::<req::Completion>(handler::completion)
            .request_snap::<req::HoverRequest>(handler::hover)
            .request_snap::<req::DocumentHighlightRequest>(handler::document_highlight)
            .request_snap::<req::References>(handler::references)
            .request_snap::<lsp_ext::SyntaxTree>(handler::syntax_tree)
            .request_snap::<req::SemanticTokensFullRequest>(handler::semantic_token_full)
            .request_snap::<req::SemanticTokensRangeRequest>(handler::semantic_token_range)
            //// Events ////
            .event(Self::on_set_package_info)
            .event(Self::on_update_config)
            .event(Self::on_update_diagnostics)
            // Loopback event.
            .event(Self::on_did_change_watched_files);
        router
    }

    pub fn new(client: ClientSocket, init_messages: Vec<ShowMessageParams>) -> Self {
        Self {
            host: AnalysisHost::default(),
            vfs: Arc::new(RwLock::new(Vfs::new())),
            opened_files: HashMap::default(),
            #[cfg(unix)]
            config: Arc::new(Config::new("/non-existing-path".into())),
            #[cfg(windows)]
            config: Arc::new(Config::new("c://non-existing-path".into())),

            gleam_interop_client: None,
            source_roots: IndexSet::new(),
            diagnostics: DiagnosticCollector {
                // client,
                external: HashMap::new(),
                internal: HashMap::new(),
            },
            client,
            init_messages,
            capabilities: NegotiatedCapabilities::default(),
        }
    }

    fn formatting(
        &mut self,
        params: DocumentFormattingParams,
    ) -> impl Future<Output = Result<Option<Vec<TextEdit>>, ResponseError>> {
        fn run_with_stdin(
            cmd: &[String],
            stdin_data: impl AsRef<[u8]> + Send + 'static,
        ) -> Result<String> {
            let mut child = process::Command::new(&cmd[0])
                .args(&cmd[1..])
                .stdin(process::Stdio::piped())
                .stdout(process::Stdio::piped())
                .stderr(process::Stdio::piped())
                .spawn()?;
            let mut stdin = child.stdin.take().unwrap();
            std::thread::spawn(move || {
                let _ = std::io::copy(&mut stdin_data.as_ref(), &mut stdin);
            });
            let output = child.wait_with_output()?;

            if !output.status.success() {
                return Err(anyhow::Error::msg("Could not format"));
            }
            tracing::info!("ERR {:?}", String::from_utf8_lossy(&output.stderr));
            ensure!(
                output.status.success(),
                "Formatter exited with {}, stderr: {}",
                output.status,
                String::from_utf8_lossy(&output.stderr),
            );
            let stdout = String::from_utf8(output.stdout)?;
            Ok(stdout)
        }

        let cmd = vec![
            String::from("gleam"),
            String::from("format"),
            String::from("--stdin"),
        ];

        let vfs = self.vfs.clone();

        async move {
            let (file_content, line_map) = {
                let vfs = vfs.read().unwrap();
                let Ok((file, line_map)) = convert::from_file(&vfs, &params.text_document) else {
                    return Ok(None);
                };
                (vfs.content_for_file(file), line_map)
            };

            let new_content = match run_with_stdin(&cmd, <Arc<[u8]>>::from(file_content.clone())) {
                Ok(result) => result,
                Err(_) => {
                    return Ok(None);
                }
            };

            if new_content == *file_content {
                return Ok(None);
            }

            // Replace the whole file.
            let last_line = line_map.last_line();
            Ok(Some(vec![TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: last_line,
                        character: line_map.end_col_for_line(last_line),
                    },
                },
                new_text: new_content,
            }]))
        }
    }

    fn on_initialize(
        &mut self,
        params: InitializeParams,
    ) -> impl Future<Output = Result<InitializeResult, ResponseError>> {
        tracing::info!("Init params: {params:?}");

        let (server_caps, final_caps) = negotiate_capabilities(&params);

        self.capabilities = final_caps;

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
            let errors = cfg.update(options);
            if !errors.is_empty() {
                let msg = ["Failed to apply some settings:"]
                    .into_iter()
                    .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                    .collect::<String>();
                self.client.show_message_ext(MessageType::ERROR, msg);
            }
        }
        *Arc::get_mut(&mut self.config).expect("No concurrent access yet") = cfg;

        let (mainloop, mut server) = async_lsp::MainLoop::new_client(|_server| {
            let client = self.client.clone();
            let router = InteropClient::new_router(client);

            ServiceBuilder::new()
                .layer(TracingLayer::default())
                .layer(CatchUnwindLayer::default())
                .layer(ConcurrencyLayer::default())
                .service(router)
        });

        let child = async_process::Command::new("gleam")
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .kill_on_drop(false)
            .spawn()
            .expect("Failed to run gleam lsp");
        let stdout = child.stdout.unwrap();
        let stdin = child.stdin.unwrap();

        let mut client = self.client.clone();

        tokio::spawn(async move {
            match mainloop.run_buffered(stdout, stdin).await {
                Ok(_) => {}
                Err(e) => client.show_message_ext(MessageType::ERROR, e.to_string()),
            };
        });
        self.gleam_interop_client = Some(server.clone());

        // Initialize.
        async move {
            let _params = server
                .initialize(InitializeParams {
                    root_uri: Some(params.root_uri.unwrap()),
                    capabilities: ClientCapabilities {
                        window: Some(WindowClientCapabilities {
                            work_done_progress: Some(true),
                            ..WindowClientCapabilities::default()
                        }),
                        ..ClientCapabilities::default()
                    },
                    ..InitializeParams::default()
                })
                .await
                .unwrap();
            server.initialized(InitializedParams {}).unwrap();

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

        let uri = params.text_document.uri.clone();
        self.opened_files.insert(uri.clone(), FileData::default());
        self.set_vfs_file_content(&uri, params.text_document.text.clone());

        let _ = self.gleam_interop_client.as_ref().unwrap().did_open(params);
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
        let Ok(file) = vfs.file_for_uri(&uri) else {
            return ControlFlow::Continue(());
        };
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

    fn on_did_save(&mut self, params: DidSaveTextDocumentParams) -> NotifyResult {
        tracing::info!("Received save notification");
        let _ = self.gleam_interop_client.as_ref().unwrap().did_save(params);
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

        for &FileEvent { ref uri, mut typ } in &params.changes {
            // Don't reload files maintained by the client.
            if self.opened_files.contains_key(uri) {
                continue;
            }
            let Ok(path) = uri.to_file_path() else {
                continue;
            };

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
        }

        ControlFlow::Continue(())
    }

    /// Spawn a task to (re)load the gleam workspace via `gleam.toml`
    // fn spawn_load_gleam_workspace(&mut self) {
    //     let fut = task::spawn(Self::load_gleam_workspace(
    //         self.vfs.clone(),
    //         self.config.clone(),
    //         self.capabilities.clone(),
    //         self.client.clone(),
    //     ));
    //     if let Some(prev_fut) = self.load_gleam_workspace_fut.replace(fut) {
    //         prev_fut.abort();
    //     }
    // }

    fn assemble_graph(
        vfs: &mut Vfs,
        root_path: &Path,
        graph: &mut PackageGraph,
        roots: &mut IndexSet<PackageRoot>,
        seen: &mut HashMap<SmolStr, PackageId>,
        is_local: bool,
    ) -> Result<PackageId> {
        roots.insert(PackageRoot {
            path: root_path.to_path_buf(),
        });

        let (gleam_file, gleam_src) = {
            let gleam_path = root_path.join(GLEAM_TOML);
            let gleam_toml_src = std::fs::read_to_string(&gleam_path)?;
            let gleam_toml_vpath = VfsPath::new(gleam_path);
            let gleam_file = vfs.set_path_content(gleam_toml_vpath, gleam_toml_src);

            let src = vfs.content_for_file(gleam_file);
            (gleam_file, src)
        };
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
            .unwrap_or("erlang")
            .into();

        let package = match seen.get(name.as_str()) {
            Some(idx) => idx.clone(),
            None => graph.add_package(name.clone(), gleam_file),
        };

        seen.insert(name, package);

        let package_dir = root_path.join("build/packages");

        tracing::info!("direct deps {:?}", direct_deps);
        for (name, dep) in direct_deps.iter() {
            if let Some(dep_id) = seen.get(name.as_str()) {
                let dependency = Dependency { package: *dep_id };
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
                    let Ok(dep_id) =
                        Self::assemble_graph(vfs, &local_path, graph, roots, seen, true)
                    else {
                        continue;
                    };
                    dep_id
                }
                None => {
                    let Ok(dep_id) = Self::assemble_graph(vfs, &path, graph, roots, seen, false)
                    else {
                        continue;
                    };
                    dep_id
                }
            };

            seen.insert(name.into(), dep_id);

            let dependency = Dependency { package: dep_id };
            graph.add_dep(package, dependency);
        }
        graph.set_target(target);

        Ok(package)
    }

    fn load_package_files(vfs: &mut Vfs, root_path: &std::path::PathBuf) {
        let walkdir = WalkDir::new(root_path)
            .follow_links(true)
            .into_iter()
            .filter_entry(|entry| {
                if !entry.file_type().is_dir() {
                    return true;
                }
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
                if ext == "gleam" || entry_path.ends_with("gleam.toml") {
                    return Some(entry_path);
                }
            }
            None
        });
        for file in files {
            if let Ok(src) = std::fs::read_to_string(file.clone()) {
                let file_id = vfs.set_path_content(VfsPath::from(file.clone()), src);
                tracing::info!("Loading file {:?} {:?}", file, file_id);
            }
        }
    }

    fn on_set_package_info(&mut self, info: SetPackageGraphEvent) -> NotifyResult {
        tracing::debug!("Set package info: {:#?}", info.0);
        let mut vfs = self.vfs.write().unwrap();

        vfs.set_package_graph(info.0.as_ref().map(|i| i.0.clone()));

        // let roots = info.0.map(|i| i.1.clone()).unwrap_or_else(|| {
        //     vec![PackageRoot {
        //         path: self.config.root_path.clone(),
        //         is_local: true,
        //     }]
        // });

        drop(vfs);

        self.apply_vfs_change();

        // This is currently mostly to get proper diagnostics on startup
        let uris = self.opened_files.keys().cloned().collect::<Vec<_>>();
        for uri in uris {
            tracing::trace!("Recalculate diagnostics of {uri}");
            self.spawn_update_diagnostics(uri.clone());
        }
        tracing::info!("Finished loading workspace!");
        // Informing loading service, that workspace has loaded
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
        let errors = config.update(value.0);
        tracing::debug!("Updated config, errors: {errors:?}, config: {config:?}");
        self.config = Arc::new(config);

        if !errors.is_empty() {
            let msg = ["Failed to apply some settings:"]
                .into_iter()
                .chain(errors.iter().flat_map(|s| ["\n- ", s]))
                .collect::<String>();
            self.client.show_message_ext(MessageType::ERROR, msg);
        }

        ControlFlow::Continue(())
    }

    fn on_update_diagnostics(&mut self, event: CollectDiagnosticsEvent) -> NotifyResult {
        let diag = match event {
            CollectDiagnosticsEvent::External(diagnostics) => {
                let uri = diagnostics.uri.clone();
                let mut new_diags = diagnostics.clone();
                self.diagnostics.external.insert(uri.clone(), diagnostics);
                let mut internal = self
                    .diagnostics
                    .internal
                    .get(&uri)
                    .map_or_else(Vec::new, |e| e.diagnostics.clone());
                new_diags.diagnostics.append(&mut internal);
                new_diags
            }
            CollectDiagnosticsEvent::Internal(diagnostics) => {
                let uri = diagnostics.uri.clone();
                let mut new_diags = diagnostics.clone();
                self.diagnostics.internal.insert(uri.clone(), diagnostics);
                let mut external = self
                    .diagnostics
                    .external
                    .get(&uri)
                    .map_or_else(Vec::new, |e| e.diagnostics.clone());
                new_diags.diagnostics.append(&mut external);
                new_diags
            }
        };
        let _ = self.client.publish_diagnostics(diag);

        ControlFlow::Continue(())
    }

    fn spawn_update_diagnostics(&mut self, uri: Url) {
        let task = self.spawn_with_snapshot({
            let uri = uri.clone();
            move |snap| {
                // Return empty diagnostics for ignored files.
                with_catch_unwind("diagnostics", || handler::diagnostics(snap, &uri))
                    .unwrap_or_else(|err| {
                        tracing::error!("Failed to calculate diagnostics: {err}");
                        Vec::new()
                    })
            }
        });

        // Can this really fail?
        let Some(f) = self.opened_files.get_mut(&uri) else {
            task.abort();
            return;
        };
        if let Some(prev_task) = f.diagnostics_task.replace(task.abort_handle()) {
            prev_task.abort();
        }

        task::spawn({
            let client = self.client.clone();
            async move {
                if let Ok(diagnostics) = task.await {
                    tracing::debug!("Publish {} diagnostics for {}", diagnostics.len(), uri);
                    let _ = client.emit(CollectDiagnosticsEvent::Internal(
                        PublishDiagnosticsParams {
                            uri,
                            diagnostics,
                            version: None,
                        },
                    ));
                    // let _: Result<_, _> = client.publish_diagnostics(PublishDiagnosticsParams {
                    //     uri,
                    //     diagnostics,
                    //     version: None,
                    // });
                } else {
                    // Task cancelled, then there must be another task queued already. Do nothing.
                }
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

    // This function is responsible for handling onOpen / didChangeFile events
    // and potentially discovering / loading new packages
    fn set_vfs_file_content(&mut self, uri: &Url, text: String) {
        let vpath = uri.to_vfs_path();

        // Find cargo toml and add to source_roots
        let mut vfs = self.vfs.write().unwrap();

        let mut source_root_changed = false;

        let mut package_roots = self.source_roots.clone();
        // if file is not loaded yet, insert package source root
        if let Err(_) = vfs.file_for_path(&vpath) {
            if let Some(path) = vpath.as_path().and_then(find_gleam_project_parent) {
                tracing::info!("Setting new sourceroot {:?}", path);
                package_roots.insert(PackageRoot { path });
                source_root_changed = true;
            }
        }

        vfs.set_path_content(vpath.clone(), text);

        // if new source has been discovered or a gleam.toml has changed, reassemble package graph
        if source_root_changed
            || vpath
                .as_path()
                .expect("Should be a path!")
                .ends_with("gleam.toml")
        {
            let mut graph = PackageGraph::default();
            for source_root in package_roots.clone().iter() {
                tracing::info!("LOADING: {:?}", vfs);
                let mut seen = HashMap::new();
                let _ = Self::assemble_graph(
                    &mut vfs,
                    &source_root.path,
                    &mut graph,
                    &mut package_roots,
                    &mut seen,
                    true,
                );
            }
            tracing::info!("setting graph {:#?}", graph);
            vfs.set_package_graph(Some(graph));
        }

        for package in &package_roots {
            let source_changed = self.source_roots.insert(package.clone());
            // If a new source_root has been inserted, load the corresponding package files
            if source_changed {
                tracing::info!("Loading package {:?}", package);
                Self::load_package_files(&mut vfs, &package.path);
            }
        }

        // if a file has been added, re-partition the roots
        if vfs.is_structural_change() {
            let source_roots = Self::lower_vfs(&mut vfs, &self.source_roots);

            tracing::info!("SR {:#?}", &source_roots);
            vfs.set_roots(source_roots);
        }
        drop(vfs);

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

    fn lower_vfs(vfs: &mut Vfs, source_root_config: &IndexSet<PackageRoot>) -> Vec<SourceRoot> {
        let mut prefix_to_paths = HashMap::new();

        let mut prefix_components = Vec::new();

        for prefix in source_root_config {
            prefix_components.push(prefix.path.components().collect::<Vec<_>>())
        }

        // sorting by length matches the longest prefix first
        // e.g. /path/to/module is matched before /path
        prefix_components.sort_by_key(|b| std::cmp::Reverse(b.len()));

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
            .map(|(path, set)| SourceRoot::new(set, path))
            .collect();
        source_roots
    }
}

fn find_gleam_project_parent(path: &Path) -> Option<PathBuf> {
    let mut is_module = path.extension().map(|x| x == "gleam").unwrap_or(false);
    let mut directory = path.to_path_buf();

    while let Some(root) = directory.parent() {
        // If there's no gleam.toml in the root then we continue to the next parent.
        if !&root.join("gleam.toml").is_file() {
            _ = directory.pop();
            continue;
        }

        // If it is a Gleam module then it must reside in the src or test directory.
        if is_module && !(directory.ends_with("test") || directory.ends_with("src")) {
            _ = directory.pop();
            continue;
        }

        if let Some(parent) = root.parent() {
            if let Some(grand_parent) = parent.parent() {
                if parent.file_name() == Some(OsStr::new("packages"))
                    && grand_parent.file_name() == Some(OsStr::new("build"))
                {
                    _ = directory.pop();
                    // we need to stop popping for no reason!
                    is_module = false;
                    continue;
                }
            }
        }

        return Some(root.to_path_buf());
    }
    None
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

    #[allow(dead_code)]
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
