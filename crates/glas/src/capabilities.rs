use lsp_types::{
    CompletionOptions, HoverProviderCapability, InitializeParams, OneOf, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};

use crate::semantic_tokens::{SEMANTIC_TOKEN_MODIFIERS, SEMANTIC_TOKEN_TYPES};

macro_rules! test {
    ($lhs:ident $(.$field:ident)*) => {
        Some($lhs)
            $(.and_then(|opt| opt.$field.as_ref()))*
        == Some(&true)
    };
}

pub(crate) fn negotiate_capabilities(
    init_params: &InitializeParams,
) -> (ServerCapabilities, NegotiatedCapabilities) {
    let client_caps = &init_params.capabilities;
    let is_neovim = init_params
        .client_info
        .as_ref()
        .map_or(false, |info| info.name == "Neovim");

    let final_caps = NegotiatedCapabilities {
        client_show_message_request: test!(
            client_caps
                .window
                .show_message
                .message_action_item
                // This is required for knowing which action is performed.
                .additional_properties_support
        ),
        server_initiated_progress: test!(client_caps.window.work_done_progress),
        watch_files: test!(
            client_caps
                .workspace
                .did_change_watched_files
                .dynamic_registration
        ),
        // Workaround: https://github.com/neovim/neovim/issues/23380
        watch_files_relative_pattern: !is_neovim
            && test!(
                client_caps
                    .workspace
                    .did_change_watched_files
                    .relative_pattern_support
            ),
        workspace_configuration: test!(client_caps.workspace.configuration),
    };

    let server_caps = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(lsp_types::TextDocumentSyncSaveOptions::Supported(true)),
            },
        )),
        references_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into(), "@".into()]),
            ..Default::default()
        }),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                    token_types: SEMANTIC_TOKEN_TYPES.to_vec(),
                    token_modifiers: SEMANTIC_TOKEN_MODIFIERS.to_vec(),
                },
                range: Some(true),
                full: None,
            },
        )),
        document_highlight_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };

    (server_caps, final_caps)
}

#[derive(Clone, Debug, Default)]
pub(crate) struct NegotiatedCapabilities {
    #[allow(dead_code)]
    pub client_show_message_request: bool,
    pub server_initiated_progress: bool,
    pub watch_files: bool,
    pub watch_files_relative_pattern: bool,
    pub workspace_configuration: bool,
}
