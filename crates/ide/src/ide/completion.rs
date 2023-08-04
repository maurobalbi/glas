use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, SourceFile},
    best_token_at_offset, find_node_at_offset, GleamLanguage, SyntaxKind,TextRange,
    TextSize, T, SyntaxNode
};

use crate::{
    def::{
        hir_def::{FunctionId, ModuleDefId},
        resolver::{resolver_for_toplevel, Resolver},
        resolver_for_expr,
        source_analyzer::find_def,
    },
    ty::TyDatabase,
    DefDatabase, FilePos, InFile,
};

use super::RootDatabase;

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
    Param,
    Pattern,
    Field,
}

pub struct CompletionContext<'db> {
    db: &'db dyn DefDatabase,
    is_top_level: bool,
    source_range: TextRange,
    expr_ptr: Option<InFile<SyntaxNode>>,
}

impl<'db> CompletionContext<'db> {
    fn new(
        db: &'db dyn DefDatabase,
        original_file: &'db SourceFile,
        position: FilePos,
    ) -> Option<CompletionContext<'db>> {
        let mut ctx = CompletionContext {
            db,
            is_top_level: false,
            source_range: TextRange::default(),
            expr_ptr: None,
        };

        let tok = best_token_at_offset(original_file.syntax(), position.pos)?;

        ctx.source_range = match tok.kind() {
            T!["."] => TextRange::empty(position.pos),
            SyntaxKind::IDENT | SyntaxKind::U_IDENT => tok.text_range(),
            _ => TextRange::empty(position.pos)
        };
        let top_node = tok
            .parent_ancestors()
            .take_while(|it| it.text_range() == ctx.source_range)
            .last();

        if let Some(SyntaxKind::SOURCE_FILE) = top_node.and_then(|t| t.parent()).map(|it| it.kind()) {
            ctx.is_top_level = true;
        }

        if let Some(expr_node) = tok.parent_ancestors().find(|it| ast::Expr::can_cast(it.kind())) {
            let expr_ptr = InFile {
                file_id: position.file_id,
                value: expr_node,
            };
            ctx.expr_ptr = Some(expr_ptr);
        }

        ctx.fill(&original_file, position.pos); // First, let's try to complete a reference to some declaration.

        Some(ctx)
    }

    fn fill(&mut self, original_file: &'db SourceFile, offset: TextSize) {
        if let Some(name_ref) = find_node_at_offset::<ast::NameRef>(original_file.syntax(), offset)
        {
            tracing::info!("im filling");
            self.classify_name_ref(original_file, &name_ref);
        }
    }

    fn classify_name_ref(&mut self, original_file: &'db SourceFile, name_ref: &ast::NameRef) {}
}

pub(crate) fn completions(
    db: &dyn DefDatabase,
    fpos @ FilePos { file_id, pos }: FilePos,
    trigger_char: Option<char>,
) -> Option<Vec<CompletionItem>> {
    let root = db.parse(file_id).root();

    tracing::info!("Completeing compl");
    let ctx = CompletionContext::new(db, &root, fpos)?;

    let mut acc = Vec::default();

    complete_snippet(&mut acc, &ctx);
    complete_expr(&mut acc, &ctx);

    Some(acc)
}

fn complete_expr(acc: &mut Vec<CompletionItem>, ctx: &CompletionContext<'_>) -> Option<()> {
    let expr_ptr = ctx.expr_ptr.clone()?;
    let resolver = match find_def(ctx.db, expr_ptr.as_ref()) {
        Some(ModuleDefId::FunctionId(id)) => {
            let source_map = ctx.db.body_source_map(id);
            let expr_ptr = expr_ptr.with_value(ast::Expr::cast(expr_ptr.value.clone())?);
            resolver_for_expr(ctx.db, id, source_map.expr_for_node(expr_ptr.as_ref())?)
        }
        _ => resolver_for_toplevel(ctx.db, expr_ptr.file_id),
    };

    for (name, def) in resolver.names_in_scope() {
        let kind =  match def {
            crate::def::resolver::ResolveResult::LocalBinding(_) => CompletionItemKind::Param,
            crate::def::resolver::ResolveResult::FunctionId(_) => CompletionItemKind::Function,
            crate::def::resolver::ResolveResult::VariantId(_) => CompletionItemKind::Variant,
        };
        acc.push(CompletionItem {
            label: name.clone(),
            source_range: ctx.source_range,
            replace: name.clone(),
            kind,
            signature: None,
            description: None,
            documentation: None,
            is_snippet: true,
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};
    use tracing_test::traced_test;

    #[track_caller]
    fn check_no(fixture: &str, label: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        if let Some(compes) = super::completions(&db, f[0], None) {
            assert_eq!(compes.iter().find(|item| item.label == label), None);
        }
    }

    #[track_caller]
    fn check_trigger(fixture: &str, trigger_char: Option<char>, label: &str, expect: Expect) {
        let (mut db, f) = TestDB::from_fixture(fixture).unwrap();

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
        let (mut db, f) = TestDB::from_fixture(fixture).unwrap();
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
        check("i$0", "import", expect!["(Keyword) import"]);
    }

    #[traced_test]
    #[test]
    fn scope() {
        check_all("fn main() { a$0 }", None, expect![""]);
    }
}
