use crate::{convert, lsp_ext::SyntaxTreeParams, StateSnapshot};
use anyhow::Result;
use ide::{FileRange, GotoDefinitionResult};
use lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverParams, Url, DocumentHighlightParams, DocumentHighlight,
};

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub(crate) fn hover(snap: StateSnapshot, params: HoverParams) -> Result<Option<Hover>> {
    let (fpos, line_map) =
        convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.hover(fpos)?;
    Ok(ret.map(|hover| convert::to_hover(&line_map, hover)))
}

pub(crate) fn completion(
    snap: StateSnapshot,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let (fpos, line_map) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let trigger_char = params
        .context
        .and_then(|ctx| ctx.trigger_character?.chars().next());
    let Some(items) = snap.analysis.completions(fpos, trigger_char)? else {
        return Ok(None);
    };
    let items = items
        .into_iter()
        .map(|item| convert::to_completion_item(&line_map, item))
        .collect::<Vec<_>>();
    Ok(Some(CompletionResponse::Array(items)))
}

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

pub(crate) fn goto_definition(
    snap: StateSnapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let (fpos, _) = convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.goto_definition(fpos)?;
    let vfs = snap.vfs();
    let targets = match ret {
        Some(GotoDefinitionResult::Targets(targets)) => targets
            .into_iter()
            .map(|target| {
                convert::to_location(&vfs, FileRange::new(target.file_id, target.focus_range))
            })
            .collect(),
        _ => return Ok(None),
    };
    Ok(Some(GotoDefinitionResponse::Array(targets)))
}

pub(crate) fn document_highlight(
    snap: StateSnapshot,
    params: DocumentHighlightParams,
) -> Result<Option<Vec<DocumentHighlight>>> {
    let (fpos, line_map) =
        convert::from_file_pos(&snap.vfs(), &params.text_document_position_params)?;
    let ret = snap.analysis.highlight_related(fpos)?;
    let ret = convert::to_document_highlight(&line_map, &ret);
    Ok(Some(ret))
}

pub(crate) fn syntax_tree(snap: StateSnapshot, params: SyntaxTreeParams) -> Result<String> {
    let (file, _) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let syntax_tree = snap.analysis.syntax_tree(file)?;
    Ok(syntax_tree)
}
