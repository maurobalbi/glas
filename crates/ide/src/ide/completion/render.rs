use smol_str::SmolStr;
use syntax::{ast, match_ast};

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
    let mut params_len = it.params(ctx.db.upcast()).len();
    let mut is_pipe_or_use = false;

    // In use or pipe add one less argument
    if let Some(ptr) = ctx.expr_ptr.clone().and_then(|ptr| ptr.value.parent()) {
        match_ast! {
           match ptr {
                ast::Pipe(_) => {
                    is_pipe_or_use = true;
                },
                ast::StmtUse(_) => {
                    is_pipe_or_use = true;
                },
                _ => {},
            }
        };
    }

    if is_pipe_or_use {
        params_len = params_len.checked_sub(1).unwrap_or(0);
    };

    let params = (1..=params_len)
        .map(|i| format!("${{{}:_}}", i))
        .collect::<Vec<_>>()
        .join(", ");

    let name = it.name(ctx.db.upcast());
    let label = if params_len > 0 {
        format!("{}(…)", name)
    } else if is_pipe_or_use {
        format!("{}", name)
    } else {
        format!("{}()", name)
    };

    let replace_params = if is_pipe_or_use && params_len == 0 {
        format!("")
    } else {
        format!("({params})")
    };

    let replace = format!("{}{}", name, replace_params);

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
