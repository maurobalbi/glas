use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, SourceFile},
    best_token_at_offset, find_node_at_offset, match_ast, GleamLanguage, SyntaxKind, SyntaxNode,
    SyntaxToken, TextRange, TextSize, T,
};

use crate::{
    def::{
        find_container,
        hir::Function,
        hir::Variant,
        hir_def::ModuleDefId,
        resolver::{resolver_for_toplevel, ResolveResult},
        resolver_for_expr, Semantics,
    },
    ty::TyDatabase,
    FilePos, InFile,
};

/// A single completion variant in the editor pop-up.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    /// The label to show in the completion menu.
    pub label: SmolStr,
    /// Range of identifier that is being completed.
    pub source_range: TextRange,
    /// What content replaces the source range when user selects this item.
    pub replace: SmolStr,
    /// What item (struct, function, etc) are we completing.
    pub kind: CompletionItemKind,
    /// Type signature.
    pub signature: Option<String>,
    /// A brief description.
    pub description: Option<String>,
    /// The detailed documentation.
    pub documentation: Option<String>,
    /// Is this a snippet.
    pub is_snippet: bool,
}

/// The type of the completion item.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompletionItemKind {
    Keyword,
    Function,
    Variant,
    Adt,
    Param,
    Module,
    Pattern,
    Field,
}

pub struct CompletionContext<'db> {
    db: &'db dyn TyDatabase,
    sema: Semantics<'db>,
    is_top_level: bool,
    source_range: TextRange,
    expr_ptr: Option<InFile<SyntaxNode>>,
    tok: SyntaxToken,
    trigger_tok: Option<SyntaxToken>,
    original_file: SyntaxNode,
}

impl<'db> CompletionContext<'db> {
    fn new(db: &'db dyn TyDatabase, position: FilePos) -> Option<CompletionContext<'db>> {
        let sema = Semantics::new(db);
        let original_file = sema.parse(position.file_id);
        let tok = best_token_at_offset(original_file.syntax(), position.pos)?;

        let trigger_tok = original_file
            .syntax()
            .token_at_offset(position.pos)
            .left_biased();

        let mut ctx = CompletionContext {
            db,
            sema,
            is_top_level: false,
            source_range: TextRange::default(),
            expr_ptr: None,
            original_file: original_file.syntax().clone(),
            tok,
            trigger_tok,
        };

        ctx.source_range = match ctx.tok.kind() {
            T!["."] => TextRange::empty(position.pos),
            SyntaxKind::IDENT | SyntaxKind::U_IDENT => ctx.tok.text_range(),
            a if a.is_keyword() => ctx.tok.text_range(),
            _ => TextRange::empty(position.pos),
        };
        let top_node = ctx
            .tok
            .parent_ancestors()
            .take_while(|it| {
                it.text_range() == ctx.source_range && !(it.kind() == SyntaxKind::SOURCE_FILE)
            })
            .last();

        if let Some(SyntaxKind::SOURCE_FILE) = top_node.and_then(|t| t.parent()).map(|it| it.kind())
        {
            ctx.is_top_level = true;
        }

        if let Some(expr_node) = ctx
            .tok
            .parent_ancestors()
            .find(|it| ast::Expr::can_cast(it.kind()))
        {
            let expr_ptr = InFile {
                file_id: position.file_id,
                value: expr_node,
            };
            ctx.expr_ptr = Some(expr_ptr);
        }

        ctx.fill(original_file, position.pos); // First, let's try to complete a reference to some declaration.

        Some(ctx)
    }

    fn fill(&mut self, original_file: SourceFile, offset: TextSize) {
        if let Some(name_ref) = find_node_at_offset::<ast::NameRef>(original_file.syntax(), offset)
        {
            self.classify_name_ref(original_file, &name_ref);
        }
    }

    fn classify_name_ref(&mut self, _original_file: SourceFile, _name_ref: &ast::NameRef) {}
}

pub(crate) fn completions(
    db: &dyn TyDatabase,
    fpos @ FilePos { file_id, pos: _ }: FilePos,
    trigger_char: Option<char>,
) -> Option<Vec<CompletionItem>> {
    let ctx = CompletionContext::new(db, fpos)?;

    let mut acc = Vec::default();

    if trigger_char == Some('.') {
        complete_dot(&mut acc, ctx);
        return Some(acc);
    }
    if trigger_char == Some('@') {
        complete_at(&mut acc, ctx);
        return Some(acc);
    }

    complete_snippet(&mut acc, &ctx);
    complete_expr(&mut acc, &ctx);
    complete_import(&mut acc, &ctx);

    Some(acc)
}

fn complete_at(acc: &mut Vec<CompletionItem>, ctx: CompletionContext<'_>) {
    acc.push(CompletionItem {
        label: "target(javscript)".into(),
        source_range: ctx.source_range,
        replace: "target(javascript)".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
    acc.push(CompletionItem {
        label: "target(erlang)".into(),
        source_range: ctx.source_range,
        replace: "target(erlang)".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
    acc.push(CompletionItem {
        label: "external(javascript, ..)".into(),
        source_range: ctx.source_range,
        replace: "external(javascript, \"$1\", \"$2\")".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
    acc.push(CompletionItem {
        label: "external(erlang, ..)".into(),
        source_range: ctx.source_range,
        replace: "external(erlang, \"$1\", \"$2\")".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
}

fn complete_dot(acc: &mut Vec<CompletionItem>, ctx: CompletionContext<'_>) -> Option<()> {
    let trigger_tok = ctx.trigger_tok?;
    match_ast! {
        match (trigger_tok.parent()?) {
            ast::FieldAccessExpr(it) => {
                // let receiver = find_opt_node_in_file(&ctx.original_file, it.base())?;
                // let ty = ctx.sema.ty_of_expr(&receiver);
                let file = ctx.sema.resolve_module(it.base()?)?;

                let module_items = ctx.db.module_scope(file);

                for (def, _) in module_items.declarations().flatten() {
                    let kind = match def {
                        ModuleDefId::FunctionId(_) => CompletionItemKind::Function,
                        ModuleDefId::AdtId(_) => CompletionItemKind::Adt,
                        ModuleDefId::VariantId(_) => CompletionItemKind::Variant,
                        ModuleDefId::TypeAliasId(_) => CompletionItemKind::Adt,
                    };
                    let (name, replace) = match def {
                        // ToDo: Extract to function because it's also used in complete_expr
                        ModuleDefId::FunctionId(it) => {
                            let it = Function { id: *it};
                            let params = it.params(ctx.db.upcast());
                            let params = params
                                .iter()
                                .enumerate()
                                .map(|(i, p)|
                                    match p.label.clone() {
                                        Some(label) => {
                                            format!("{}: ${{{}:{}}}", label, i + 1, p.name)
                                        }
                                        None => {
                                            format!("${{{}:{}}}", i + 1, p.name)
                                        }
                                    }
                                )
                                .collect::<Vec<_>>()
                                .join(", ");
                            let name = it.name(ctx.db.upcast());
                            let label = if params.len() > 0 {
                                format!("{}(…)", name)
                            } else {
                                format!("{}()", name)
                            };
                            (label, format!("{}({})", name, params))
                        }
                        ModuleDefId::VariantId(it) => {
                            let it = Variant { parent: it.parent, id: it.local_id };
                            let fields = it.fields(ctx.db.upcast());
                            let fields_str = fields
                                .iter()
                                .enumerate()
                                .map(|(i, p)|
                                    format!("${{{}:{}}}", i + 1, p.label.clone().unwrap_or("()".into()))
                                )
                                .collect::<Vec<_>>()
                                .join(", ");
                            let name = it.name(ctx.db.upcast());
                            let label = if fields.len() > 0 {
                                format!("{}(…)", name)
                            } else {
                                format!("{}()", name)
                            };
                            (label, format!("{}({})", name, fields_str))
                        },
                        _ => { continue }
                    };
                    acc.push(CompletionItem {
                        label: name.into(),
                        source_range: ctx.source_range,
                        replace: replace.into(),
                        kind: kind,
                        signature: Some(String::from("signature")),
                        description: Some(String::from("desription")),
                        documentation: Some(String::from("docs")),
                        is_snippet: false,
                    });


                }

            },
            // type_name_ref comes here also!
            _ => return None,
        }
    }
    Some(())
}

fn complete_import(acc: &mut Vec<CompletionItem>, ctx: &CompletionContext<'_>) -> Option<()> {
    if ctx
        .tok
        .parent_ancestors()
        .find(|t| ast::ModulePath::can_cast(t.kind()))
        .is_some()
    {
        for (_, name) in ctx.db.module_map().iter() {
            acc.push(CompletionItem {
                label: name.clone(),
                source_range: ctx.source_range,
                replace: name.clone(),
                kind: CompletionItemKind::Module,
                signature: None,
                description: None,
                documentation: None,
                is_snippet: false,
            })
        }
    }
    Some(())
}

fn complete_expr(acc: &mut Vec<CompletionItem>, ctx: &CompletionContext<'_>) -> Option<()> {
    let expr_ptr = ctx.expr_ptr.clone()?;
    let resolver = match find_container(ctx.db.upcast(), expr_ptr.as_ref()) {
        Some(ModuleDefId::FunctionId(id)) => {
            let source_map = ctx.db.body_source_map(id);
            let expr_ptr = expr_ptr.with_value(ast::Expr::cast(expr_ptr.value.clone())?);
            resolver_for_expr(
                ctx.db.upcast(),
                id,
                source_map.expr_for_node(expr_ptr.as_ref())?,
            )
        }
        _ => resolver_for_toplevel(ctx.db.upcast(), expr_ptr.file_id),
    };

    for (name, def) in resolver.names_in_scope() {
        let kind = match def {
            ResolveResult::Module(_) => CompletionItemKind::Module,
            ResolveResult::Local(_) => CompletionItemKind::Param,
            ResolveResult::Function(_) => CompletionItemKind::Function,
            ResolveResult::Variant(_) => CompletionItemKind::Variant,
            ResolveResult::BuiltIn(_) => CompletionItemKind::Variant,
        };
        let replace = match def {
            ResolveResult::Function(it) => {
                let params = it.params(ctx.db.upcast());
                let params = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| match p.label.clone() {
                        Some(label) => format!("{}: ${{{}:{}}}", label, i + 1, p.name),
                        None => format!("${{{}:{}}}", i + 1, p.name),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", name, params)
            }
            _ => format!("{}", name),
        };
        acc.push(CompletionItem {
            label: name.clone(),
            source_range: ctx.source_range,
            replace: replace.into(),
            kind,
            signature: None,
            description: None,
            documentation: None,
            is_snippet: true,
        })
    }

    let module_data = ctx.db.module_items(expr_ptr.file_id);

    for (_, import) in module_data.module_imports() {
        acc.push(CompletionItem {
            label: import.accessor.clone(),
            source_range: ctx.source_range,
            replace: import.accessor.clone(),
            kind: CompletionItemKind::Module,
            signature: None,
            description: None,
            documentation: None,
            is_snippet: false,
        })
    }

    Some(())
}

fn complete_snippet(acc: &mut Vec<CompletionItem>, ctx: &CompletionContext) {
    if !ctx.is_top_level {
        return;
    };
    acc.push(CompletionItem {
        label: "fn".into(),
        source_range: ctx.source_range,
        replace: "fn ${1:name}($2) { $0 }".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: Some("fn name() { }".into()),
        documentation: None,
        is_snippet: true,
    });
    acc.push(CompletionItem {
        label: "import".into(),
        source_range: ctx.source_range,
        replace: "import ${1:module}".into(),
        kind: CompletionItemKind::Keyword,
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
}

/// Attempts to find `node` inside `syntax` via `node`'s text range.
/// If the fake identifier has been inserted after this node or inside of this node use the `_compensated` version instead.
fn find_opt_node_in_file<N: AstNode<Language = GleamLanguage>>(
    syntax: &SyntaxNode,
    node: Option<N>,
) -> Option<N> {
    find_node_in_file(syntax, &node?)
}

/// Attempts to find `node` inside `syntax` via `node`'s text range.
/// If the fake identifier has been inserted after this node or inside of this node use the `_compensated` version instead.
fn find_node_in_file<N: AstNode<Language = GleamLanguage>>(
    syntax: &SyntaxNode,
    node: &N,
) -> Option<N> {
    let syntax_range = syntax.text_range();
    let range = node.syntax().text_range();
    let intersection = range.intersect(syntax_range)?;
    syntax
        .covering_element(intersection)
        .ancestors()
        .find_map(N::cast)
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str, label: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        if let Some(compes) = super::completions(&db, f[0], None) {
            assert_eq!(compes.iter().find(|item| item.label == label), None);
        }
    }

    #[track_caller]
    fn check_trigger(fixture: &str, trigger_char: Option<char>, label: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();

        let compes = super::completions(&db, f[0], trigger_char).expect("No completion");
        let item = compes
            .iter()
            .find(|item| item.label == label)
            .expect("No expected completion");

        let source_range =
            usize::from(item.source_range.start())..usize::from(item.source_range.end());
        let mut completed = db.file_content(f[0].file_id).to_string();
        completed.replace_range(source_range, &item.replace);
        let got = format!("({:?}) {}", item.kind, completed);
        expect.assert_eq(&got);
    }

    #[track_caller]
    fn check_all(fixture: &str, trigger_char: Option<char>, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        let compes = super::completions(&db, f[0], trigger_char)
            .expect("No completion")
            .iter()
            .map(|c| format!("({:?}) {}", c.kind, c.label))
            .collect::<Vec<_>>()
            .join(";");
        expect.assert_eq(&compes);
    }

    #[track_caller]
    fn check(fixture: &str, label: &str, expect: Expect) {
        check_trigger(fixture, None, label, expect);
    }

    #[test]
    fn keyword() {
        check("i$0", "import", expect!["(Keyword) import ${1:module}"]);
    }

    #[test]
    fn scope() {
        check_all("fn main() { a$0 }", None, expect!["(Function) main"]);
    }
}
