use crate::{def::{hir_def::{FunctionId, AdtId}, hir::{Function, Adt, Module}}, CompletionItem, CompletionItemKind, ty::display::TyDisplay, FileId, CompletionRelevance};

use super::CompletionContext;

pub fn render_fn(ctx: &CompletionContext<'_>, id: &FunctionId) -> CompletionItem {
    let it = Function { id: *id };
    let params = it.params(ctx.db.upcast());
    let params = params
        .iter()
        .enumerate()
        .map(|(i, p)| match p.label.clone() {
            Some(label) => {
                format!("{}: ${{{}:{}}}", label, i + 1, p.name)
            }
            None => {
                format!("${{{}:{}}}", i + 1, p.name)
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    let name = it.name(ctx.db.upcast());
    let label = if !params.is_empty() {
        format!("{}(…)", name)
    } else {
        format!("{}()", name)
    };

    let replace = format!("{}({})", name, params);

    let docs = it.docs(ctx.db.upcast());

    CompletionItem {
        label: label.into(),
        source_range: ctx.source_range,
        replace: replace.into(),
        kind: CompletionItemKind::Function,
        signature: Some(it.ty(ctx.db).display(ctx.db).to_string()),
        relevance: CompletionRelevance::default(),
        description: None,
        documentation: Some(docs),
        is_snippet: false,
    }
}

pub fn render_adt(ctx: &CompletionContext<'_>, id: &AdtId) -> CompletionItem {
    let it = Adt { id: *id };
    let name = it.name(ctx.db.upcast());
    let label = format!("{}(…)", name);

    let docs = it.docs(ctx.db.upcast());

    CompletionItem {
        label: label.clone().into(),
        source_range: ctx.source_range,
        replace: label.into(),
        kind: CompletionItemKind::Function,
        relevance: CompletionRelevance::default(),
        signature: None,
        description: None,
        documentation: Some(docs),
        is_snippet: false,
    }
}

pub fn render_module(ctx: &CompletionContext<'_>, id: &FileId) -> CompletionItem {
    let it = Module { id: *id };
    let label = it.import_accessor(ctx.db.upcast());

    let docs = it.docs(ctx.db.upcast());

    CompletionItem {
        label: label.clone().into(),
        source_range: ctx.source_range,
        replace: label.into(),
        kind: CompletionItemKind::Module,
        relevance: CompletionRelevance::default(),
        signature: Some(format!("import {}", it.name(ctx.db.upcast()).to_string())),
        description: None,
        documentation: Some(docs),
        is_snippet: false,
    }
}