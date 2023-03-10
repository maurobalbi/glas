use crate::{convert, StateSnapshot};
use anyhow::Result;
use lsp_types::{Diagnostic, Url};

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub(crate) fn diagnostics(snap: StateSnapshot, uri: &Url) -> Result<Vec<Diagnostic>> {
    let (file, line_map) = {
        let vfs = snap.vfs();
        let file = vfs.file_for_uri(uri)?;
        (file, vfs.line_map_for_file(file))
    };
    let mut diags = snap.analysis.diagnostics(file)?;
    diags.retain(|diag| !snap.config.diagnostics_ignored.contains(diag.code()));
    diags.truncate(MAX_DIAGNOSTICS_CNT);
    Ok(convert::to_diagnostics(uri, file, &line_map, &diags))
}
