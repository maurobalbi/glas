use lsp_types::ServerCapabilities;

pub(crate) fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        ..Default::default()
    }
}
