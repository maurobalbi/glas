mod render;

use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, SourceFile},
    best_token_at_offset, find_node_at_offset, match_ast, SyntaxKind, SyntaxNode, SyntaxToken,
    TextRange, TextSize, T,
};

use crate::{
    def::{
        find_container,
        hir::{Adt, Module, Package},
        hir_def::ModuleDefId,
        module::Visibility,
        resolver::{resolver_for_toplevel, ResolveResult},
        resolver_for_expr, Semantics,
    },
    ty::{display::TyDisplay, Ty, TyDatabase},
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
    pub relevance: CompletionRelevance,
    /// The detailed documentation.
    pub documentation: Option<String>,
    /// Is this a snippet.
    pub is_snippet: bool,
}

impl CompletionRelevance {
    /// Provides a relevance score. Higher values are more relevant.
    ///
    /// The absolute value of the relevance score is not meaningful, for
    /// example a value of 0 doesn't mean "not relevant", rather
    /// it means "least relevant". The score value should only be used
    /// for relative ordering.
    ///
    /// See is_relevant if you need to make some judgement about score
    /// in an absolute sense.
    pub fn score(self) -> u32 {
        let mut score = 0;
        let CompletionRelevance {
            exact_name_match,
            type_match,
            is_local,
        } = self;

        if exact_name_match {
            score += 10;
        }
        score += match type_match {
            Some(CompletionRelevanceTypeMatch::Exact) => 8,
            Some(CompletionRelevanceTypeMatch::CouldUnify) => 3,
            None => 0,
        };
        // slightly prefer locals
        if is_local {
            score += 1;
        }
        score
    }

    /// Returns true when the score for this threshold is above
    /// some threshold such that we think it is especially likely
    /// to be relevant.
    pub fn is_relevant(&self) -> bool {
        self.score() > 0
    }
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub struct CompletionRelevance {
    pub exact_name_match: bool,
    pub type_match: Option<CompletionRelevanceTypeMatch>,
    pub is_local: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum CompletionRelevanceTypeMatch {
    CouldUnify,
    Exact,
}

pub struct CompletionContext<'db> {
    db: &'db dyn TyDatabase,
    sema: Semantics<'db>,
    package: Package,
    is_top_level: bool,
    source_range: TextRange,
    expr_ptr: Option<InFile<SyntaxNode>>,
    tok: SyntaxToken,
    trigger_tok: Option<SyntaxToken>,
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

        let module = Module {
            id: position.file_id,
        };

        let mut ctx = CompletionContext {
            db,
            sema,
            is_top_level: false,
            source_range: TextRange::default(),
            package: module.package(db.upcast()),
            expr_ptr: None,
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
                it.text_range() == ctx.source_range && it.kind() != SyntaxKind::SOURCE_FILE
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
    fpos: FilePos,
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
        relevance: CompletionRelevance::default(),
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
        relevance: CompletionRelevance::default(),
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
        relevance: CompletionRelevance::default(),
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
        relevance: CompletionRelevance::default(),
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
}

fn complete_dot(acc: &mut Vec<CompletionItem>, ctx: CompletionContext<'_>) -> Option<()> {
    let trigger_tok = ctx.trigger_tok.clone()?;
    match_ast! {
        match (trigger_tok.parent()?) {
            ast::FieldAccessExpr(it) => {
                let analyzer = ctx.sema.analyze(it.base()?.syntax())?;
                let ty = analyzer.type_of_expr(&it.base()?);

                if let Some(Ty::Adt { adt_id, .. }) = ty {
                    let fields = Adt {id: adt_id}.common_fields(ctx.db.upcast());
                    for (name, field) in fields {
                        acc.push(CompletionItem {
                            label: name.clone(),
                            source_range: ctx.source_range,
                            replace: name,
                            kind: CompletionItemKind::Field,
                            relevance: CompletionRelevance::default(),
                            signature: Some(format!("{}", field.ty(ctx.db.upcast()))),
                            description: None,
                            documentation: None,
                            is_snippet: false,
                        })
                    }
                };

                let file = ctx.sema.resolve_module(it.base()?)?;

                let module_items = ctx.db.module_scope(file);

                for (def, _) in module_items.declarations().flatten().filter(|item| item.1 != Visibility::Private) {
                    match def {
                        ModuleDefId::FunctionId(it) => {
                            acc.push(render::render_fn(&ctx, it));
                        }
                        ModuleDefId::VariantId(it) => {
                           acc.push(render::render_variant(&ctx, it))
                        },
                        _ => { continue }
                    };
                    // acc.push(CompletionItem {
                    //     label: name.into(),
                    //     source_range: ctx.source_range,
                    //     replace: replace.into(),
                    //     kind,
                    //     signature: Some(String::from("signature")),
                    //     description: Some(String::from("desription")),
                    //     documentation: Some(String::from("docs")),
                    //     is_snippet: false,
                    // });


                }

            },
            // type_name_ref comes here also!
            _ => return None,
        }
    }
    Some(())
}

fn complete_import(acc: &mut Vec<CompletionItem>, ctx: &CompletionContext<'_>) -> Option<()> {
    if let Some(module_path) = ctx
        .tok
        .parent_ancestors()
        .find_map(|t| ast::ModulePath::cast(t))
    {
        // ToDo: get all visible modules
        for (_, name) in ctx.package.visible_modules(ctx.db.upcast()).iter() {
            acc.push(CompletionItem {
                label: name.clone(),
                source_range: module_path.syntax().text_range(),
                replace: name.clone(),
                kind: CompletionItemKind::Module,
                relevance: CompletionRelevance::default(),
                signature: None,
                description: None,
                documentation: None,
                is_snippet: false,
            })
        }
    };
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

    for (name, def) in resolver.values_names_in_scope() {
        let kind = match def {
            ResolveResult::Module(_) => CompletionItemKind::Module,
            ResolveResult::ModuleConstant(_) => CompletionItemKind::Function,
            ResolveResult::Local(_) => CompletionItemKind::Param,
            ResolveResult::Function(_) => CompletionItemKind::Function,
            ResolveResult::Variant(_) => CompletionItemKind::Variant,
            ResolveResult::BuiltIn(_) => CompletionItemKind::Variant,
            ResolveResult::Adt(_) => CompletionItemKind::Adt,
            ResolveResult::TypeAlias(_) => CompletionItemKind::Adt,
        };
        match def {
            ResolveResult::Function(it) => {
                acc.push(render::render_fn(ctx, &it.id));
            }
            ResolveResult::Variant(it) => acc.push(render::render_variant(ctx, &it.into())),
            ResolveResult::Local(it) => {
                let mut relevance = CompletionRelevance::default();
                relevance.is_local = true;

                acc.push(CompletionItem {
                    label: name.clone(),
                    source_range: ctx.source_range,
                    replace: format!("{}", name).into(),
                    kind,
                    signature: Some(it.ty(ctx.db).display(ctx.db).to_string()),
                    relevance: relevance,
                    description: None,
                    documentation: None,
                    is_snippet: false,
                })
            }
            _ => acc.push(CompletionItem {
                label: name.clone(),
                source_range: ctx.source_range,
                replace: format!("{}", name).into(),
                relevance: CompletionRelevance::default(),
                kind,
                signature: None,
                description: None,
                documentation: None,
                is_snippet: true,
            }),
        };
    }

    let module_data = ctx.db.module_items(expr_ptr.file_id);

    for (_, import) in module_data.module_imports() {
        let module =
            resolver.resolve_module(&import.as_name.as_ref().unwrap_or_else(|| &import.accessor));

        module.map(|id| acc.push(render::render_module(ctx, &id, import.as_name.clone())));
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
        relevance: CompletionRelevance::default(),
        is_snippet: true,
    });
    acc.push(CompletionItem {
        label: "import".into(),
        source_range: ctx.source_range,
        replace: "import ${1:module}".into(),
        kind: CompletionItemKind::Keyword,
        relevance: CompletionRelevance::default(),
        signature: None,
        description: None,
        documentation: None,
        is_snippet: true,
    });
}

#[cfg(test)]
mod tests {
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[allow(dead_code)]
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
    fn check(fixture: &str, label: &str, expect: Expect) {
        check_trigger(fixture, None, label, expect);
    }

    #[test]
    fn keyword() {
        check("i$0", "import", expect!["(Keyword) import ${1:module}"]);
    }

    #[test]
    fn scope() {
        check(
            "fn main() { a$0 }",
            "main",
            expect!["(Function) fn main() { main }"],
        );
    }
}
