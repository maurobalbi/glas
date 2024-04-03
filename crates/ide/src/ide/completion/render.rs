use smol_str::SmolStr;

use crate::{
    def::{
        hir::{Adt, Function, Module, Variant},
        hir_def::{FunctionId, VariantId},
    },
    ty::display::TyDisplay,
    CompletionItem, CompletionItemKind, CompletionRelevance, FileId,
};

use super::CompletionContext;

pub fn render_fn(ctx: &CompletionContext<'_>, id: &FunctionId) -> CompletionItem {
    let it = Function { id: *id };

    let name = it.name(ctx.db.upcast());

    let docs = it.docs(ctx.db.upcast());

    CompletionItem {
        label: name.clone().into(),
        source_range: ctx.source_range,
        replace: name.into(),
        kind: CompletionItemKind::Function,
        signature: Some(it.ty(ctx.db).display(ctx.db).to_string()),
        relevance: CompletionRelevance::default(),
        description: None,
        documentation: Some(docs),
        is_snippet: false,
    }
}

pub fn render_variant(ctx: &CompletionContext<'_>, id: &VariantId) -> CompletionItem {
    let it = Variant {
        parent: id.parent,
        id: id.local_id,
    };

    let name = it.name(ctx.db.upcast());

    let docs = it.docs(ctx.db.upcast());

    CompletionItem {
        label: name.clone().into(),
        source_range: ctx.source_range,
        replace: name.into(),
        kind: CompletionItemKind::Function,
        signature: Some(Adt { id: it.parent }.name(ctx.db.upcast()).into()),
        relevance: CompletionRelevance::default(),
        description: None,
        documentation: Some(docs),
        is_snippet: false,
    }
}

pub fn render_module(
    ctx: &CompletionContext<'_>,
    id: &FileId,
    as_name: Option<SmolStr>,
) -> CompletionItem {
    let it = Module { id: *id };
    let label = as_name.unwrap_or_else(|| it.import_accessor(ctx.db.upcast()));

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
