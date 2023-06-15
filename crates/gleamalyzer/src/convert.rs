use crate::{LineMap, Result, Vfs};
use async_lsp::{ResponseError, ErrorCode};
use ide::{Diagnostic, FileId, FilePos, FileRange, Severity, DiagnosticKind};
use lsp::DiagnosticTag;
use lsp_types::{
    self as lsp, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, PrepareRenameResponse, Range, TextDocumentIdentifier, TextDocumentPositionParams,
    Url,
};
use std::sync::Arc;
use text_size::{TextRange, TextSize};

pub(crate) fn from_file(vfs: &Vfs, doc: &TextDocumentIdentifier) -> Result<(FileId, Arc<LineMap>)> {
    let file = vfs.file_for_uri(&doc.uri)?;
    let line_map = vfs.line_map_for_file(file);
    Ok((file, line_map))
}

pub(crate) fn from_pos(line_map: &LineMap, pos: Position) -> Result<TextSize> {
    Ok(line_map.pos_for_line_col(pos.line, pos.character))
}

pub(crate) fn from_file_pos(
    vfs: &Vfs,
    params: &TextDocumentPositionParams,
) -> Result<(FilePos, Arc<LineMap>)> {
    let (file, line_map) = from_file(vfs, &params.text_document)?;
    let pos = from_pos(&line_map, params.position)?;
    Ok((FilePos::new(file, pos), line_map))
}

pub(crate) fn from_range(
    vfs: &Vfs,
    file: FileId,
    range: Range,
) -> Result<(Arc<LineMap>, TextRange)> {
    let line_map = vfs.line_map_for_file(file);
    let start = from_pos(&line_map, range.start)?;
    let end = from_pos(&line_map, range.end)?;
    Ok((line_map, TextRange::new(start, end)))
}

pub(crate) fn to_location(vfs: &Vfs, frange: FileRange) -> Location {
    let uri = vfs.uri_for_file(frange.file_id);
    let line_map = vfs.line_map_for_file(frange.file_id);
    Location::new(uri, to_range(&line_map, frange.range))
}

pub(crate) fn to_range(line_map: &LineMap, range: TextRange) -> Range {
    let (line1, col1) = line_map.line_col_for_pos(range.start());
    let (line2, col2) = line_map.line_col_for_pos(range.end());
    Range::new(Position::new(line1, col1), Position::new(line2, col2))
}

pub(crate) fn to_diagnostics(
    uri: &Url,
    file: FileId,
    line_map: &LineMap,
    diags: &[Diagnostic],
) -> Vec<lsp::Diagnostic> {
    let mut ret = Vec::with_capacity(diags.len() * 2);
    for diag in diags {
        let primary_diag = lsp::Diagnostic {
            severity: match diag.severity() {
                Severity::Error | Severity::IncompleteSyntax => Some(DiagnosticSeverity::ERROR),
                Severity::Warning => Some(DiagnosticSeverity::WARNING),
                Severity::Info => Some(DiagnosticSeverity::HINT),
            },
            range: to_range(line_map, diag.range),
            code: Some(NumberOrString::String(diag.code().into())),
            code_description: None,
            source: None,
            message: diag.message(),
            related_information: {
                Some(
                    diag.notes
                        .iter()
                        .map(|(frange, msg)| DiagnosticRelatedInformation {
                            location: Location::new(uri.clone(), to_range(line_map, frange.range)),
                            message: msg.to_owned(),
                        })
                        .collect(),
                )
            },
            tags: {
                let mut tags = Vec::new();
                if diag.kind == DiagnosticKind::InactiveTarget {
                  tags.push(DiagnosticTag::UNNECESSARY);
                }
                Some(tags)
            },
            data: None,
        };

        // Hoist related information to top-level Hints.
        for (frange, msg) in &diag.notes {
            // We cannot handle cross-file diagnostics here.
            if frange.file_id != file {
                continue;
            }

            ret.push(lsp::Diagnostic {
                severity: Some(DiagnosticSeverity::HINT),
                range: to_range(line_map, frange.range),
                code: primary_diag.code.clone(),
                code_description: primary_diag.code_description.clone(),
                source: primary_diag.source.clone(),
                message: msg.into(),
                related_information: Some(vec![DiagnosticRelatedInformation {
                    location: Location::new(uri.clone(), to_range(line_map, diag.range)),
                    message: "original diagnostic".into(),
                }]),
                tags: None,
                data: None,
            });
        }

        ret.push(primary_diag);
    }

    ret
}

pub(crate) fn to_rename_error(message: String) -> ResponseError {
    ResponseError::new(ErrorCode::REQUEST_FAILED, message)
}

pub(crate) fn to_prepare_rename_response(
    line_map: &LineMap,
    range: TextRange,
    text: String,
) -> PrepareRenameResponse {
    let range = to_range(line_map, range);
    PrepareRenameResponse::RangeWithPlaceholder {
        range,
        placeholder: text,
    }
}
