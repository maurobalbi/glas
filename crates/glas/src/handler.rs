use std::{process, sync::Arc};

use crate::{convert, lsp_ext::SyntaxTreeParams, StateSnapshot};
use anyhow::{ensure, Context, Result};
use ide::{FileRange, GotoDefinitionResult};
use lsp_types::{
    CompletionParams, CompletionResponse, Diagnostic, DocumentFormattingParams, DocumentHighlight,
    DocumentHighlightParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams,
    Location, Position, Range, ReferenceParams, SemanticTokens, SemanticTokensParams,
    SemanticTokensRangeParams, SemanticTokensRangeResult, SemanticTokensResult, TextEdit, Url,
};

const MAX_DIAGNOSTICS_CNT: usize = 128;

pub(crate) fn formatting(
    snap: StateSnapshot,
    params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
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

    let (file_content, line_map) = {
        let vfs = snap.vfs();
        let (file, line_map) = convert::from_file(&vfs, &params.text_document)?;
        (vfs.content_for_file(file), line_map)
    };

    let Ok(new_content) = run_with_stdin(&cmd, <Arc<[u8]>>::from(file_content.clone())) else {    
        return Ok(None);
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
    let max_relevance = items
        .iter()
        .map(|it| it.relevance.score())
        .max()
        .unwrap_or_default();
    let items = items
        .into_iter()
        .map(|item| convert::to_completion_item(&line_map, max_relevance, item))
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

pub(crate) fn references(
    snap: StateSnapshot,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let (fpos, _) = convert::from_file_pos(&snap.vfs(), &params.text_document_position)?;
    let Some(refs) = snap.analysis.references(fpos)? else {
        return Ok(None);
    };
    let vfs = snap.vfs();
    let locs = refs
        .into_iter()
        .map(|frange| convert::to_location(&vfs, frange))
        .collect::<Vec<_>>();
    Ok(Some(locs))
}

pub(crate) fn syntax_tree(snap: StateSnapshot, params: SyntaxTreeParams) -> Result<String> {
    let (file, _) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let syntax_tree = snap.analysis.syntax_tree(file)?;
    Ok(syntax_tree)
}

pub(crate) fn semantic_token_full(
    snap: StateSnapshot,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let (file, line_map) = convert::from_file(&snap.vfs(), &params.text_document)?;
    let hls = snap.analysis.syntax_highlight(file, None)?;
    let toks = convert::to_semantic_tokens(&line_map, &hls);
    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: toks,
    })))
}

pub(crate) fn semantic_token_range(
    snap: StateSnapshot,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    let (file, range, line_map) = {
        let vfs = snap.vfs();
        let (file, line_map) = convert::from_file(&vfs, &params.text_document)?;
        let (_, range) = convert::from_range(&vfs, file, params.range)?;
        (file, range, line_map)
    };
    let hls = snap.analysis.syntax_highlight(file, Some(range))?;
    let toks = convert::to_semantic_tokens(&line_map, &hls);
    Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data: toks,
    })))
}
