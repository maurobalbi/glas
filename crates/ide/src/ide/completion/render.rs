use smol_str::SmolStr;

use crate::{
    def::{
        hir::{Adt, Function, Module, Variant},
        hir_def::{AdtId, FunctionId, VariantId},
    },
    ty::display::TyDisplay,
    CompletionItem, CompletionItemKind, CompletionRelevance, FileId,
};

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

pub fn render_variant(ctx: &CompletionContext<'_>, id: &VariantId) -> CompletionItem {
    let it = Variant {
        parent: id.parent,
        id: id.local_id,
    };
    let fields = it.fields(ctx.db.upcast());
    let _fields_str = fields
        .iter()
        .enumerate()
        .map(|(i, p)| {
            format!(
                "${{{}:{}}}",
                i + 1,
                p.label(ctx.db.upcast()).clone().unwrap_or("()".into())
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    let name = it.name(ctx.db.upcast());
    let label = if !fields.is_empty() {
        format!("{}(…)", name)
    } else {
        format!("{}", name)
    };

    let docs = it.docs(ctx.db.upcast());
    CompletionItem {
        label: label.into(),
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
