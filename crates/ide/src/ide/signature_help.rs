use itertools::Itertools;
use syntax::{ast, match_ast, rowan::Direction, NodeOrToken, SyntaxToken, TextRange, TextSize, T};
use tracing_subscriber::fmt::format;

use crate::{
    def::Semantics,
    ty::{self, display::TyDisplay, TyDatabase},
    DefDatabase, FilePos,
};
use syntax::ast::AstNode;

/// Contains information about an item signature as seen from a use site.
///
/// This includes the "active parameter", which is the parameter whose value is currently being
/// edited.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    pub doc: Option<String>,
    pub signature: String,
    pub active_parameter: Option<usize>,
    parameters: Vec<TextRange>,
}

impl SignatureHelp {
    pub fn parameter_labels(&self) -> impl Iterator<Item = &str> + '_ {
        self.parameters.iter().map(move |&it| &self.signature[it])
    }

    pub fn parameter_ranges(&self) -> &[TextRange] {
        &self.parameters
    }

    fn push_param(&mut self, opening_delim: &str, param: &str) {
        if !self.signature.ends_with(opening_delim) {
            self.signature.push_str(", ");
        }
        let start = TextSize::of(&self.signature);
        self.signature.push_str(param);
        let end = TextSize::of(&self.signature);
        self.parameters.push(TextRange::new(start, end))
    }
}

/// Computes parameter information for the given position.
pub(crate) fn signature_help(
    db: &dyn TyDatabase,
    FilePos { file_id, pos }: FilePos,
) -> Option<SignatureHelp> {
    let sema = Semantics::new(db);
    let parse = sema.parse(file_id);
    let token = parse
        .syntax()
        .token_at_offset(pos)
        .left_biased()
        // if the cursor is sandwiched between two space tokens and the call is unclosed
        // this prevents us from leaving the CallExpression
        .and_then(|tok| syntax::skip_trivia_token(tok, Direction::Prev))?;

    for node in token.parent_ancestors() {
        match_ast! {
            match node {
                ast::ArgList(arg_list) => {
                    let cursor_outside = arg_list.r_paren_token().as_ref() == Some(&token);
                    if cursor_outside {
                        continue;
                    }
                    let parent = arg_list.syntax().parent()?;
                    if let Some(func) = ast::ExprCall::cast(parent).and_then(|expr| expr.func()) {
                        return signature_help_for_call(&sema, func, arg_list, token);
                    }

                    // return signature_help_for_record(&sema, arg_list, token);
                },
                _ => (),
            }
        }
    }
    None
}

fn signature_help_for_call(
    sema: &Semantics<'_>,
    expr: ast::Expr,
    arg_list: ast::ArgList,
    token: SyntaxToken,
) -> Option<SignatureHelp> {
    let arg_list_children = arg_list.syntax().children_with_tokens();
    let mut active_parameter = arg_list_children
        .filter_map(NodeOrToken::into_token)
        .filter(|t| t.kind() == T![","])
        .take_while(|t| t.text_range().start() <= token.text_range().start())
        .count();

    let mut is_pipe = false;
    let mut is_use = false;

    // In use or pipe add one less argument
    if let Some(ptr) = expr.syntax().parent().and_then(|p| p.parent()) {
        match_ast! {
           match ptr {
                ast::Pipe(_) => {
                    is_pipe = true;
                    active_parameter += 1;
                },
                ast::StmtUse(_) => {
                    is_use = true;
                },
                _ => {},
            }
        };
    };

    let resolved = sema.analyze(&token.parent()?)?.type_of_expr(&expr)?;

    let ty::Ty::Function { params, return_ } = resolved else {
        return None;
    };

    let mut res = SignatureHelp {
        doc: None,
        signature: String::from("("),
        parameters: vec![],
        active_parameter: Some(active_parameter),
    };

    let mut params2_mut = params.as_ref().clone();

    for (idx1, arg) in arg_list.args().enumerate() {
        if let Some((idx2, _)) = params2_mut.iter().find_position(|(label2, _)| {
            label2.is_some() && *label2 == arg.label().and_then(|l| l.text())
        }) {
            if is_pipe {
                move_element(&mut params2_mut, idx2, idx1 + 1)
            } else {
                move_element(&mut params2_mut, idx2, idx1)
            }
        }
    }

    for (idx, param) in params2_mut.clone().iter().enumerate() {
        let parameter = match &param.0 {
            Some(label) => {
                format!("{}: {}", label, param.1.display(sema.db))
            }
            None => format!("{}", param.1.display(sema.db)),
        };
        if is_pipe && idx == 0 {
            res.signature.push_str("|> ");
            res.push_param("|> ", &parameter.as_str());
        } else {
            res.push_param("(", &parameter.as_str());
        }

        if is_use && idx == params2_mut.len() - 1 {
            res.signature.push_str(" <- use");
        }
    }

    let ret = format!(") -> {}", return_.display(sema.db));
    res.signature.push_str(ret.as_str());
    tracing::info!(
        "sh para{:?} {:?} {:?}",
        params2_mut,
        res.parameters,
        active_parameter
    );

    Some(res)
}

fn move_element<T>(vec: &mut Vec<T>, from_index: usize, to_index: usize) {
    if from_index == to_index {
        return;
    } // No operation needed if indices are the same

    // Ensure indices are within bounds
    let len = vec.len();
    if !(from_index < len && to_index < len) {
        return;
    }

    // Remove the element from the original position
    let element = vec.remove(from_index);

    // Insert the element at the new position
    vec.insert(to_index, element);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::base::SourceDatabase;
    use crate::tests::TestDB;
    use expect_test::{expect, Expect};

    #[track_caller]
    fn check_no(fixture: &str) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        assert_eq!(signature_help(&db, f[0]), None);
    }

    #[track_caller]
    fn check(fixture: &str, expect: Expect) {
        let (db, f) = TestDB::from_fixture(fixture).unwrap();
        assert_eq!(f.markers().len(), 1, "Missing markers");
        let help = signature_help(&db, f[0]).expect("No definition");

        let mut got = help.signature;

        let active_range = help
            .parameters
            .get(help.active_parameter.expect("No active parameter"))
            .expect("Parameter idx not in range");

        got.insert(active_range.end().into(),'>');
        got.insert(active_range.start().into(), '<');

        expect.assert_eq(&got);
    }

    #[test]
    fn pipe_signature() {
        check(r#"
        fn labelled(label1 arg1: Int, label2 arg2: String) { 
            arg2
        }

        fn main() {
            1 |> labelled($0)
        }"#, expect![
            r#"(|> label1: Int, <label2: String>) -> String"#
        ]);

        check(r#"
        fn labelled(label1 arg1: Int, label2 arg2: String) { 
            arg2
        }

        fn main() {
            1 |> labelled(label1: $0)
        }"#, expect![
            r#"(|> label2: String, <label1: Int>) -> String"#
        ])
    }

    #[test]
    fn fn_signature() {
        check(r#"
        fn labelled(label1 arg1: Int, label2 arg2: String) { 
            arg2
        }

        fn main() {
            labelled(label2: "123", $0)
        }"#, expect![
            r#"(label2: String, <label1: Int>) -> String"#
        ])
    }
}
