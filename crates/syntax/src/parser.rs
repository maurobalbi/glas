use std::cell::Cell;

use crate::ast::{AstNode, SourceFile, NameRef};
use crate::lexer::{GleamLexer, LexToken};
use crate::token_set::TokenSet;
use crate::SyntaxKind::{self, *};
use crate::{Error, ErrorKind, SyntaxNode};
use rowan::{GreenNode, GreenNodeBuilder, TextRange, TextSize};

const STMT_RECOVERY: TokenSet = TokenSet::new(&[
    T!["fn"],
    T!["type"],
    T!["import"],
    T!["const"],
    T!["pub"],
    T!["if"],
]);
const STMT_EXPR_RECOVERY: TokenSet = TokenSet::new(&[T!["let"], T!["use"]]).union(STMT_RECOVERY);

const PARAM_LIST_RECOVERY: TokenSet = TokenSet::new(&[T!["->"], T!["{"]]).union(STMT_RECOVERY);
const GENERIC_PARAM_LIST_RECOVERY: TokenSet =
    TokenSet::new(&[T!["{"], T!["="]]).union(STMT_RECOVERY);
const IMPORT_RECOVERY: TokenSet = TokenSet::new(&[T!["as"]]).union(STMT_RECOVERY);
const PATTERN_RECOVERY: TokenSet = TokenSet::new(&[T!["->"], T!["="]]).union(STMT_RECOVERY);

const PATTERN_FIRST: TokenSet = TokenSet::new(&[
    IDENT,
    U_IDENT,
    DISCARD_IDENT,
    INTEGER,
    FLOAT,
    STRING,
    T!["<<"],
    T!["["],
    T!["#"],
    T!["-"],
]);
const TYPE_FIRST: TokenSet = TokenSet::new(&[T!["fn"], T!["#"], IDENT, U_IDENT]);
const CONST_FIRST: TokenSet = TokenSet::new(&[IDENT, T!["#"], T!["["], INTEGER, FLOAT, STRING]);
const EXPR_FIRST: TokenSet = TokenSet::new(&[
    IDENT,
    U_IDENT,
    DISCARD_IDENT,
    T!["-"],
    T!["!"],
    T!["panic"],
    T!["todo"],
    INTEGER,
    FLOAT,
    STRING,
    T!["#"],
    T!["<<"],
    T!["["],
    T!["{"],
    T!["case"],
    T!["fn"],
]);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parse {
    green: GreenNode,
    errors: Vec<Error>,
}

impl Parse {
    pub fn green_node(&self) -> GreenNode {
        self.green.clone()
    }

    pub fn root(&self) -> SourceFile {
        SourceFile::cast(self.syntax_node()).unwrap()
    }

    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

pub fn parse_module(src: &str) -> Parse {
    assert!(src.len() < u32::MAX as usize);
    let tokens_raw: Vec<_> = GleamLexer::new(src).collect();
    let tokens = tokens_raw
        .clone()
        .into_iter()
        .filter(|&t| !t.kind.is_trivia())
        .collect();
    let mut p = Parser {
        tokens,
        tokens_raw,
        errors: Vec::new(),
        src,
        pos: 0,
        fuel: Cell::new(256),
        events: Vec::new(),
    };
    module(&mut p);
    p.build_tree()
}

#[derive(Debug)]
enum Event {
    Open { kind: SyntaxKind },
    Close,
    Advance,
}

struct MarkOpened {
    index: usize,
}

struct MarkClosed {
    index: usize,
}

struct Parser<'i> {
    tokens: Vec<LexToken<'i>>,
    tokens_raw: Vec<LexToken<'i>>,
    pos: usize,
    src: &'i str,
    fuel: Cell<u32>,
    errors: Vec<Error>,
    events: Vec<Event>,
}

// This is very hackish to intersperce whitespace, but it's nice to not have to think about whitespace in the parser
// refactor next time this needs to be changed..
impl<'i> Parser<'i> {
    fn build_tree(self) -> Parse {
        let mut builder = GreenNodeBuilder::default();
        let tokens = self.tokens_raw;
        let mut events = self.events;
        let len = tokens.len();

        events.pop();

        let mut pos = 0;

        macro_rules! n_tokens {
            ($ident: ident) => {
                (pos..len)
                    .take_while(|&it| tokens.get(it).unwrap().kind.$ident())
                    .count()
            };
        }

        let eat_token = |n, builder: &mut GreenNodeBuilder, pos: &mut usize| {
            for _ in 0..n {
                let LexToken { kind, range, .. } = tokens.get(*pos).unwrap();
                builder.token((*kind).into(), &self.src[*range]);
                *pos += 1;
            }
        };

        for event in events {
            match event {
                Event::Open { kind } => match kind {
                    SOURCE_FILE => {
                        builder.start_node(kind.into());

                        let n_ws = n_tokens!(is_module_doc);
                        eat_token(n_ws, &mut builder, &mut pos);
                    }
                    FUNCTION | MODULE_CONSTANT => {
                        let n_ws = n_tokens!(is_whitespace);
                        eat_token(n_ws, &mut builder, &mut pos);

                        builder.start_node(kind.into());

                        let n_trivias = n_tokens!(is_stmt_doc);
                        eat_token(n_trivias, &mut builder, &mut pos);
                    }
                    _ => {
                        let n_trivias = n_tokens!(is_trivia);
                        eat_token(n_trivias, &mut builder, &mut pos);

                        builder.start_node(kind.into());
                    }
                },
                Event::Close => {
                    builder.finish_node();
                }
                Event::Advance => {
                    let n_trivias = n_tokens!(is_trivia);
                    eat_token(n_trivias + 1, &mut builder, &mut pos);
                }
            }
        }
        let n_trivias = n_tokens!(is_trivia);
        eat_token(n_trivias, &mut builder, &mut pos);
        builder.finish_node();

        Parse {
            green: builder.finish(),
            errors: self.errors,
        }
    }

    fn error(&mut self, kind: ErrorKind) {
        let range = self
            .tokens
            .get(self.pos)
            .map(|&LexToken { range, .. }| range)
            .unwrap_or_else(|| TextRange::empty(TextSize::from(self.src.len() as u32)));
        self.errors.push(Error { range, kind });
    }

    fn start_node(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: SyntaxKind::ERROR,
        });
        mark
    }

    fn start_node_before(&mut self, m: MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: m.index };
        self.events.insert(
            m.index,
            Event::Open {
                kind: SyntaxKind::ERROR,
            },
        );
        mark
    }

    fn finish_node(&mut self, m: MarkOpened, kind: SyntaxKind) -> MarkClosed {
        self.events[m.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: m.index }
    }

    fn bump(&mut self) {
        assert!(!self.eof());
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn bump_with_error(&mut self, kind: ErrorKind) {
        let m = self.start_node();
        self.error(kind);
        self.bump();
        self.finish_node(m, ERROR);
    }

    fn eof(&self) -> bool {
        self.pos == self.tokens.len()
    }

    /// Ignores whitespace
    fn nth(&self, lookahead: usize) -> SyntaxKind {
        if self.fuel.get() == 0 {
            panic!("parser is stuck")
        }
        self.fuel.set(self.fuel.get() - 1);
        self.tokens
            .get(self.pos + lookahead)
            .map_or(SyntaxKind::EOF, |it| it.kind)
    }

    fn at(&self, kind: SyntaxKind) -> bool {
        self.nth(0) == kind
    }

    fn at_any(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.nth(0))
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: SyntaxKind) {
        if self.eat(kind) {
            return;
        }
        self.error(ErrorKind::ExpectToken(kind));
    }
}

fn module(p: &mut Parser) {
    let m = p.start_node();
    while !p.eof() {
        stmnt_or_tg(p)
    }
    p.finish_node(m, SOURCE_FILE);
}

fn stmnt_or_tg(p: &mut Parser) {
    let m = p.start_node();
    match p.nth(0) {
        T!["if"] => target_group(p),
        _ => statement(p),
    }

    p.finish_node(m, TARGET_GROUP);
}

fn target_group(p: &mut Parser) {
    assert!(p.at(T!["if"]));
    p.expect(T!["if"]);
    let t = p.start_node();
    p.expect(IDENT);
    p.finish_node(t, TARGET);
    if p.at(T!["{"]) {
        statements_block(p);
    }
}

fn statements_block(p: &mut Parser) {
    assert!(p.at(T!["{"]));
    // let m = p.start_node();
    p.expect(T!["{"]);
    while !p.at(T!["}"]) && !p.eof() {
        statement(p);
    }
    p.expect(T!["}"]);
    // p.finish_node(m, STATEMENTS);
}

fn statement(p: &mut Parser) {
    let m = p.start_node();
    //parse attribute
    let is_pub = p.eat(T!["pub"]);

    match p.nth(0) {
        T!["const"] => module_const(p, m),
        T!["fn"] => {
            function(p, m, false);
        }
        T!["import"] => {
            if is_pub {
                p.bump_with_error(ErrorKind::UnexpectedImport);
            } else {
                import(p, m);
            }
        }
        T!["type"] | T!["opaque"] => custom_type(p, m),
        _ => {
            p.bump_with_error(ErrorKind::ExpectedStatement);
            p.finish_node(m, ERROR);
        }
    }
}

fn custom_type(p: &mut Parser, m: MarkOpened) {
    let opaque = p.at(T!["opaque"]);
    if opaque {
        p.expect(T!["opaque"]);
    }
    p.expect(T!["type"]);
    let t = p.start_node();
    p.expect(U_IDENT);
    p.finish_node(t, TYPE_NAME);
    if p.at(T!["("]) {
        // parse generic args
        let pl = p.start_node();
        p.expect(T!["("]);
        while !p.at(T![")"]) && !p.eof() {
            if p.at(IDENT) {
                let g = p.start_node();
                p.expect(IDENT);
                p.finish_node(g, TYPE_NAME);
                if !p.at(T![")"]) {
                    p.expect(T![","]);
                }
            } else {
                if p.at_any(GENERIC_PARAM_LIST_RECOVERY) {
                    break;
                }
                p.bump_with_error(ErrorKind::ExpectedParameter)
            }
        }
        p.expect(T![")"]);
        p.finish_node(pl, GENERIC_PARAM_LIST);
    }

    match p.nth(0) {
        T!["{"] => {
            p.expect(T!["{"]);
            while !p.at(T!["}"]) && !p.eof() {
                if p.at(U_IDENT) {
                    custom_type_variant(p);
                } else {
                    if p.at_any(STMT_RECOVERY) {
                        break;
                    }
                    p.bump_with_error(ErrorKind::ExpectedType);
                }
            }
            p.expect(T!["}"]);
            p.finish_node(m, ADT);
        }
        T!["="] => {
            if opaque {
                p.error(ErrorKind::OpaqueAlias)
            }
            p.expect(T!["="]);
            if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
                type_expr(p);
            } else {
                p.error(ErrorKind::ExpectedType)
            }
            p.finish_node(m, CUSTOM_TYPE_ALIAS);
        }
        _ => {
            p.error(ErrorKind::ExpectedType);
            p.finish_node(m, ERROR);
        }
    }
}

fn custom_type_variant(p: &mut Parser) {
    assert!(p.at(U_IDENT));
    let m = p.start_node();
    let n = p.start_node();
    p.expect(U_IDENT);
    p.finish_node(n, NAME);
    if p.at(T!["("]) {
        p.expect(T!["("]);
        let f = p.start_node();
        while !p.at(T![")"]) && !p.eof() {
            if p.at_any(TYPE_FIRST) {
                type_variant_field(p);
                if !p.at(T![")"]) {
                    p.expect(T![","]);
                }
            } else {
                if p.at_any(STMT_RECOVERY) {
                    break;
                }
                p.bump_with_error(ErrorKind::ExpectedType);
            }
        }
        p.finish_node(f, CONSTRUCTOR_FIELD_LIST);
        p.expect(T![")"]);
    }

    p.finish_node(m, VARIANT);
}

fn type_variant_field(p: &mut Parser) {
    let m = p.start_node();
    if p.nth(1) == T![":"] {
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, LABEL);
        p.expect(T![":"]);
    }
    type_expr(p);
    p.finish_node(m, CONSTRUCTOR_FIELD);
}

fn function(p: &mut Parser, m: MarkOpened, is_anon: bool) -> MarkClosed {
    assert!(p.at(T!["fn"]));
    p.expect(T!["fn"]);
    if !is_anon {
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, NAME);
    }
    if p.at(T!["("]) {
        param_list(p, is_anon);
    }

    // UX: when user is typing '-' error could be nicer
    if p.eat(T!["->"]) {
        if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
            type_expr(p);
        } else {
            p.error(ErrorKind::ExpectedType);
        }
    }

    if p.at(T!["{"]) {
        block(p);
    }

    if is_anon {
        return p.finish_node(m, LAMBDA);
    }
    p.finish_node(m, FUNCTION)
}

fn param_list(p: &mut Parser, is_anon: bool) {
    assert!(p.at(T!["("]));
    let m = p.start_node();
    p.expect(T!["("]);

    while !p.at(T![")"]) && !p.eof() {
        if p.at(IDENT) {
            param(p, is_anon);
        } else {
            if p.at_any(PARAM_LIST_RECOVERY) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedParameter)
        }
    }
    p.expect(T![")"]);
    p.finish_node(m, PARAM_LIST);
}

fn param(p: &mut Parser, is_anon: bool) {
    assert!(p.at(IDENT));
    let m = p.start_node();
    if p.nth(1) == IDENT {
        if is_anon {
            p.error(ErrorKind::UnexpectedLabel);
        }
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, LABEL);
    }
    let pat = p.start_node();
    if p.at(IDENT) || p.at(DISCARD_IDENT) {
        p.bump()
    } else {
        p.error(ErrorKind::ExpectedIdentifier);
    }
    p.finish_node(pat, PATTERN_VARIABLE);

    if p.at(T![":"]) {
        p.expect(T![":"]);
        type_expr(p);
    }
    if !p.at(T![")"]) {
        p.expect(T![","]);
    }

    p.finish_node(m, PARAM);
}

fn block(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["{"]));
    let m = p.start_node();
    p.expect(T!["{"]);
    while !p.at(T!["}"]) && !p.eof() {
        match p.nth(0) {
            T!["let"] => stmt_let(p),
            T!["use"] => stmt_use(p),
            _ => {
                if p.at_any(EXPR_FIRST) {
                    stmt_expr(p)
                } else {
                    if p.at_any(STMT_RECOVERY) {
                        break;
                    }
                    p.bump_with_error(ErrorKind::ExpectedStatement);
                }
                // p.bump_with_error(ErrorKind::ExpectedStatement)
            }
        }
    }
    p.expect(T!["}"]);
    p.finish_node(m, BLOCK)
}

fn stmt_use(p: &mut Parser) {
    assert!(p.at(T!["use"]));
    let m = p.start_node();
    p.expect(T!["use"]);
    while !p.at(T!["<-"]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            let pat_m = p.start_node();
            pattern(p);
            if p.eat(T![":"]) {
                type_expr(p);
            }
            p.finish_node(pat_m, USE_ASSIGNMENT);
            if !p.at(T!["<-"]) {
                p.expect(T![","]);
            }
        } else {
            if p.at_any(STMT_EXPR_RECOVERY) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedIdentifier);
        }
    }
    p.expect(T!["<-"]);
    if p.at_any(EXPR_FIRST) {
        expr(p);
    } else {
        p.error(ErrorKind::ExpectedExpression);
    }
    p.finish_node(m, STMT_USE);
}

fn stmt_expr(p: &mut Parser) {
    let m = p.start_node();
    expr(p);
    p.finish_node(m, STMT_EXPR);
}

fn stmt_let(p: &mut Parser) {
    assert!(p.at(T!["let"]));
    let m = p.start_node();
    p.expect(T!["let"]);
    if p.at(T!["assert"]) {
        p.expect(T!["assert"]);
    }
    pattern(p);
    if p.at(T![":"]) {
        p.expect(T![":"]);
        type_expr(p);
    }

    p.expect(T!["="]);
    if p.at_any(EXPR_FIRST) {
        expr(p);
    } else {
        p.error(ErrorKind::ExpectedExpression);
    }
    p.finish_node(m, STMT_LET);
}

fn expr(p: &mut Parser) {
    expr_bp(p, 0)
}

fn expr_bp(p: &mut Parser, min_bp: u8) {
    // let Some(mut lhs) = expr_unit(p) else {
    //     return;
    // };

    let Some(mut lhs) = (match p.nth(0).prefix_bp() {
        Some(rbp) => {
            let m = p.start_node();
            p.bump(); // Prefix op.
            expr_bp(p, rbp);
            Some(p.finish_node(m, UNARY_OP))
        }
        _ => expr_unit(p),
    }) else {
        return;
    };

    loop {
        match p.nth(0) {
            T!["("] => {
                let m = p.start_node_before(lhs);
                arg_list(p);
                lhs = p.finish_node(m, EXPR_CALL);
            }
            T!["."] => {
                p.expect(T!["."]);
                match p.nth(0) {
                    IDENT => {
                        let m = p.start_node_before(lhs);
                        let n = p.start_node();
                        p.bump();
                        p.finish_node(n, NAME_REF);
                        lhs = p.finish_node(m, FIELD_ACCESS);
                    }
                    INTEGER => {
                        let m = p.start_node_before(lhs);
                        p.bump();
                        lhs = p.finish_node(m, TUPLE_INDEX);
                    }
                    _ => {
                        let m = p.start_node_before(lhs);
                        lhs = p.finish_node(m, FIELD_ACCESS);
                        break;
                    }
                }
            }
            _ => break,
        }
    }

    loop {
        let right = p.nth(0);

        let (lbp, rbp) = match right.infix_bp() {
            None => break,
            Some(bps) => bps,
        };
        if lbp == min_bp {
            p.error(ErrorKind::MultipleNoAssoc);
            break;
        }
        if lbp < min_bp {
            break;
        }

        let m = p.start_node_before(lhs);
        p.bump(); // Infix op.
        expr_bp(p, rbp);
        if right == T!["|>"] {
            lhs = p.finish_node(m, PIPE);
        } else {
            lhs = p.finish_node(m, BINARY_OP);
        }
    }
}

// The cases that match EXPR_FIRST have to consume a token, otherwise the parser might get stuck
fn expr_unit(p: &mut Parser) -> Option<MarkClosed> {
    let res = match p.nth(0) {
        INTEGER | FLOAT | STRING => {
            let m = p.start_node();
            p.bump();
            p.finish_node(m, LITERAL)
        }
        IDENT => {
            let m = p.start_node();
            p.bump();
            p.finish_node(m, NAME_REF)
        }
        U_IDENT => {
            let b = p.start_node();
            let m = p.start_node();
            p.bump();
            p.finish_node(m, NAME_REF);
            if p.at(T!["("]) {
                arg_list(p);
            }
            p.finish_node(b, VARIANT_CONSTRUCTOR)
        }
        DISCARD_IDENT => {
            let m = p.start_node();
            p.bump();
            p.finish_node(m, HOLE)
        }
        T!["<<"] => bit_string(p),
        T!["{"] => block(p),
        T!["#"] => tuple(p),
        T!["["] => list(p),
        T!["case"] => case(p),
        T!["fn"] => {
            let m = p.start_node();
            function(p, m, true)
        }
        _ => return None,
    };
    Some(res)
}

// ToDo: Parse bit string correctly
fn bit_string(p: &mut Parser<'_>) -> MarkClosed {
    assert!(p.at(T!["<<"]));
    p.expect(T!["<<"]);
    let m = p.start_node();
    while !p.at(T![">>"]) && !p.eof() {
        p.bump();
        if p.at_any(STMT_RECOVERY) {
            break;
        }
    }
    p.expect(T![">>"]);
    p.finish_node(m, BIT_STRING)
}

fn case(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["case"]));
    let m = p.start_node();
    p.expect(T!["case"]);
    while !p.at(T!["{"]) && !p.eof() {
        if p.at_any(EXPR_FIRST) {
            expr(p);
            if !p.at(T!["{"]) {
                p.expect(T![","])
            }
        } else {
            if p.at_any(STMT_RECOVERY) {
                break;
            }
            p.error(ErrorKind::ExpectedExpression);
        }
    }
    p.expect(T!["{"]);

    while !p.at(T!["}"]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            clause(p);
        } else {
            if p.at_any(EXPR_FIRST.union(STMT_RECOVERY)) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    p.expect(T!["}"]);

    p.finish_node(m, CASE)
}

fn clause(p: &mut Parser) {
    let m = p.start_node();
    while !p.at(T!["->"]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            alternative_pattern(p);
            if !p.at(T!["->"]) && !p.at(T!["if"]) {
                p.expect(T![","]);
            }
        } else {
            if p.at_any(PATTERN_RECOVERY) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    if p.at(T!["if"]) {
        // parse guards
        p.expect(T!["if"]);
    }

    p.expect(T!["->"]);
    if p.at_any(EXPR_FIRST) {
        expr(p);
    } else {
        p.error(ErrorKind::ExpectedExpression)
    }

    p.finish_node(m, CLAUSE);
}

fn alternative_pattern(p: &mut Parser) {
    let m = p.start_node();
    while !p.at(T!["->"]) && !p.at(T![","]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            pattern(p);
            if p.at(T!["|"]) {
                p.expect(T!["|"])
            }
        } else {
            if p.at_any(EXPR_FIRST) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    p.finish_node(m, ALTERNATIVE_PATTERN);
}

fn pattern(p: &mut Parser) {
    let mut parse_application = false;

    let res = match p.nth(0) {
        // variable definition or qualified constructor type
        IDENT => {
            let m = p.start_node();
            // let n = p.start_node();
            p.expect(IDENT);
            if !p.at(T!["."]) {
                // p.finish_node(n, NAME);
                p.finish_node(m, PATTERN_VARIABLE);
                return;
            }

            parse_application = true;
            let module_m = p.finish_node(m, MODULE_NAME);
            let t_ref = p.start_node_before(module_m);
            p.expect(T!["."]);
            let t = p.start_node();
            p.expect(U_IDENT);
            p.finish_node(t, TYPE_NAME);
            p.finish_node(t_ref, TYPE_NAME_REF)
        }
        // constructor
        U_IDENT => {
            parse_application = true;
            let m = p.start_node();
            let t = p.start_node();
            p.expect(U_IDENT);
            p.finish_node(t, TYPE_NAME);
            p.finish_node(m, TYPE_NAME_REF)
        }
        DISCARD_IDENT => {
            let m = p.start_node();
            p.bump();
            p.finish_node(m, HOLE)
        }
        INTEGER | FLOAT | STRING => {
            let m = p.start_node();
            p.bump();
            p.finish_node(m, LITERAL)
        }
        T!["<<"] => bit_string(p),
        T!["["] => pattern_list(p),
        T!["-"] => todo!("unary"),
        // tuple
        T!("#") => pattern_tuple(p),
        _ => return,
    };

    if !parse_application {
        return;
    }

    match p.nth(0) {
        T!["("] => {
            let m = p.start_node_before(res);
            pattern_constructor_arg_list(p);
            p.finish_node(m, PATTERN_CONSTRUCTOR_APPLICATION);
        }
        _ => {}
    }
}

fn pattern_list(p: &mut Parser<'_>) -> MarkClosed {
    assert!(p.at(T!["["]));
    let m = p.start_node();

    p.expect(T!["["]);
    while !p.at(T!["]"]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            pattern(p);
            if !p.at(T!["]"]) {
                p.expect(T![","]);
            }
        } else {
            if p.at_any(STMT_RECOVERY) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    p.expect(T!["]"]);

    p.finish_node(m, PATTERN_LIST)
}

fn pattern_constructor_arg_list(p: &mut Parser) {
    assert!(p.at(T!["("]));
    let m = p.start_node();

    p.expect(T!["("]);
    while !p.at(T![")"]) && !p.eof() {
        if p.at_any(PATTERN_FIRST) {
            pattern_constructor_arg(p);
        } else {
            if p.at_any(STMT_RECOVERY) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    p.expect(T![")"]);

    p.finish_node(m, PATTERN_CONSTRUCTOR_ARG_LIST);
}

fn pattern_constructor_arg(p: &mut Parser) {
    let m = p.start_node();
    if !p.at_any(PATTERN_FIRST) {
        p.error(ErrorKind::ExpectedExpression);
    }
    pattern(p);
    if !p.at(T![")"]) {
        p.expect(T![","]);
    }
    p.finish_node(m, PATTERN_CONSTRUCTOR_ARG);
}

fn pattern_tuple(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["#"]));
    let m = p.start_node();
    p.expect(T!["#"]);
    p.expect(T!["("]);
    while !p.eof() && !p.at(T![")"]) {
        if p.at_any(PATTERN_FIRST) {
            pattern(p);
            if !p.at(T![")"]) {
                p.expect(T![","]);
            }
        } else {
            if p.at_any(PATTERN_FIRST) {
                break;
            }
            p.bump_with_error(ErrorKind::ExpectedExpression)
        }
    }
    p.expect(T![")"]);
    p.finish_node(m, PATTERN_TUPLE)
}

fn list(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["["]));
    let m = p.start_node();

    p.expect(T!["["]);
    while !p.at(T!["]"]) && !p.eof() {
        if p.at_any(EXPR_FIRST) {
            expr(p);
            if !p.at(T!["]"]) {
                p.expect(T![","])
            }
        } else {
            break;
        }
    }

    p.expect(T!["]"]);
    p.finish_node(m, LIST)
}

fn arg_list(p: &mut Parser) {
    assert!(p.at(T!["("]));
    let m = p.start_node();

    p.expect(T!["("]);
    while !p.at(T![")"]) && !p.eof() {
        if p.at_any(EXPR_FIRST) {
            arg(p);
        } else {
            break;
        }
    }

    p.expect(T![")"]);
    p.finish_node(m, ARG_LIST);
}

fn arg(p: &mut Parser) {
    let m = p.start_node();
    if p.nth(1) == T![":"] {
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, LABEL);
        p.expect(T![":"]);
    }
    if !p.at_any(EXPR_FIRST) {
        p.error(ErrorKind::ExpectedExpression);
    }
    expr(p);
    if !p.at(T![")"]) {
        p.expect(T![","]);
    }
    p.finish_node(m, ARG);
}

fn import(p: &mut Parser, m: MarkOpened) {
    assert!(p.at(T!["import"]));
    p.expect(T!["import"]);
    let mut parsed_ident = false;

    while !p.at_any(STMT_RECOVERY) && !p.at(T!["."]) && !p.eof() {
        parsed_ident = true;
        let n = p.start_node();
        if !p.eat(IDENT) {
            p.bump_with_error(ErrorKind::ExpectedIdentifier);
        }
        p.finish_node(n, PATH);
        if p.at(T!["/"]) {
            parsed_ident = false;
            p.bump();
        } else {
            break;
        }
    }
    if !parsed_ident {
        p.error(ErrorKind::ExpectedIdentifier);
    }

    if p.at(T!["."]) {
        unqualified_imports(p);
    }

    if p.at(T!["as"]) {
        p.bump();
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, NAME);
    }

    p.finish_node(m, IMPORT);
}

fn unqualified_imports(p: &mut Parser) {
    assert!(p.at(T!["."]));
    p.expect(T!["."]);
    p.expect(T!["{"]);
    while !p.eof() && !p.at(T!["}"]) {
        match p.nth(0) {
            IDENT => as_name(p),
            // U_IDENT => type_name(p) ToDo!
            U_IDENT => as_type_name(p),
            k if IMPORT_RECOVERY.contains(k) => break,
            _ => p.bump_with_error(ErrorKind::ExpectedParameter),
        }
    }
    p.expect(T!["}"]);
}

fn as_name(p: &mut Parser) {
    assert!(p.at(IDENT));
    let m = p.start_node();
    name(p);
    if p.at(T!["as"]) {
        p.expect(T!["as"]);
        let n = p.start_node();
        p.expect(IDENT);
        p.finish_node(n, NAME);
    }
    if !p.at(T!["}"]) {
        p.expect(T![","]);
    }
    p.finish_node(m, UNQUALIFIED_IMPORT);
}

fn as_type_name(p: &mut Parser) {
    assert!(p.at(U_IDENT));
    let m = p.start_node();
    type_name(p);
    if p.at(T!["as"]) {
        p.expect(T!["as"]);
        let n = p.start_node();
        p.expect(U_IDENT);
        p.finish_node(n, TYPE_NAME);
    }
    if !p.at(T!["}"]) {
        p.expect(T![","]);
    }
    p.finish_node(m, UNQUALIFIED_IMPORT);
}

fn name(p: &mut Parser) {
    assert!(p.at(IDENT));
    let m = p.start_node();
    p.expect(IDENT);
    p.finish_node(m, NAME);
}

fn type_name(p: &mut Parser) -> MarkClosed {
    assert!(p.at(U_IDENT));
    let m = p.start_node();
    p.expect(U_IDENT);
    p.finish_node(m, TYPE_NAME)
}

fn type_name_ref(p: &mut Parser) -> MarkClosed {
    assert!(p.at(U_IDENT));
    let m = p.start_node();
    let t = p.start_node();
    p.expect(U_IDENT);
    p.finish_node(t, TYPE_NAME);
    p.finish_node(m, TYPE_NAME_REF)
}

fn module_const(p: &mut Parser, m: MarkOpened) {
    assert!(p.at(T!["const"]));
    p.bump();
    let n = p.start_node();
    p.expect(IDENT);
    p.finish_node(n, NAME);
    if p.at(T![":"]) {
        p.expect(T![":"]);
        type_expr(p);
    }
    p.expect(T!["="]);
    const_expr(p);
    p.finish_node(m, MODULE_CONSTANT);
}

fn const_expr(p: &mut Parser) {
    match p.nth(0) {
        INTEGER | FLOAT | STRING => {
            let n = p.start_node();
            p.bump();
            p.finish_node(n, LITERAL);
        }
        T!["{"] => {
            block(p);
        }
        IDENT | U_IDENT => {
            let n = p.start_node();
            p.bump();
            p.finish_node(n, NAME_REF);
        }
        T!["#"] => {
            const_tuple(p);
        }
        _ => p.error(ErrorKind::ExpectedConstantExpression),
    };
}

fn const_tuple(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["#"]));
    let m = p.start_node();
    p.expect(T!["#"]);
    p.expect(T!["("]);
    while !p.eof() && !p.at(T![")"]) {
        if p.at_any(CONST_FIRST) {
            const_expr(p);
            if !p.at(T![")"]) {
                p.expect(T![","]);
            }
        } else {
            break;
        }
    }
    p.expect(T![")"]);
    p.finish_node(m, CONSTANT_TUPLE)
}

fn tuple(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["#"]));
    let m = p.start_node();
    p.expect(T!["#"]);
    p.expect(T!["("]);
    while !p.eof() && !p.at(T![")"]) {
        if p.at_any(EXPR_FIRST) {
            expr(p);
            if !p.at(T![")"]) {
                p.expect(T![","]);
            }
        } else {
            break;
        }
    }
    p.expect(T![")"]);
    p.finish_node(m, TUPLE)
}

fn type_arg_list(p: &mut Parser) {
    assert!(p.at(T!["("]));
    let m = p.start_node();

    p.expect(T!["("]);
    while !p.at(T![")"]) && !p.eof() {
        // Lookahead to not continue parsing if a function definition follows.
        // e.g. type Wobble =
        //      fn main() {}
        if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
            type_arg(p);
        } else {
            break;
        }
    }
    p.expect(T![")"]);

    p.finish_node(m, TYPE_ARG_LIST);
}

fn type_arg(p: &mut Parser) {
    let m = p.start_node();
    if !p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
        p.error(ErrorKind::ExpectedExpression);
    }
    type_expr(p);
    if !p.at(T![")"]) {
        p.expect(T![","]);
    }
    p.finish_node(m, TYPE_ARG);
}

fn type_expr(p: &mut Parser) {
    let mut type_application = false;
    let res = match p.nth(0) {
        T!["fn"] => fn_type(p),
        // type variable or constructor type
        IDENT => {
            let m = p.start_node();
            p.expect(IDENT);
            if !p.at(T!["."]) {
                p.finish_node(m, TYPE_NAME);
                return;
            }
            let m = p.finish_node(m, MODULE_NAME);
            let n = p.start_node_before(m);
            type_application = true;
            p.expect(T!["."]);
            let t = p.start_node();
            p.expect(U_IDENT);
            p.finish_node(t, TYPE_NAME);
            p.finish_node(n, TYPE_NAME_REF)
        }
        // constructor
        U_IDENT => {
            type_application = true;
            type_name_ref(p)
        }
        // tuple
        T!("#") => tuple_type(p),
        _ => {
            return;
            // p.bump_with_error(ErrorKind::ExpectedType);
        }
    };
    if !type_application {
        return;
    }

    match p.nth(0) {
        T!["("] => {
            let m = p.start_node_before(res);
            type_arg_list(p);
            p.finish_node(m, TYPE_APPLICATION);
        }
        _ => {}
    }
}

fn tuple_type(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["#"]));
    let m = p.start_node();
    p.expect(T!["#"]);
    p.expect(T!["("]);
    while !p.eof() && !p.at(T![")"]) {
        if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
            type_expr(p);
            if !p.at(T![")"]) {
                p.expect(T![","]);
            }
        } else {
            break;
        }
    }
    p.expect(T![")"]);
    p.finish_node(m, TUPLE_TYPE)
}

fn fn_type(p: &mut Parser) -> MarkClosed {
    assert!(p.at(T!["fn"]));
    let m = p.start_node();
    p.expect(T!["fn"]);
    let n = p.start_node();
    p.expect(T!["("]);
    while !p.at(T![")"]) && !p.eof() {
        if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
            type_expr(p);
            if !p.at(T![")"]) {
                p.expect(T![","]);
            }
        } else {
            break;
        }
    }
    p.finish_node(n, PARAM_TYPE_LIST);

    p.expect(T![")"]);
    p.expect(T!["->"]);
    if p.at_any(TYPE_FIRST) && p.nth(1) != IDENT {
        type_expr(p);
    } else {
        p.error(ErrorKind::ExpectedType);
    }
    p.finish_node(m, FN_TYPE)
}

impl SyntaxKind {
    fn prefix_bp(self) -> Option<u8> {
        Some(match self {
            T!["!"] => 17,
            T!["-"] => 18,
            _ => return None,
        })
    }

    fn infix_bp(self) -> Option<(u8, u8)> {
        Some(match self {
            T!["||"] => (1, 2),
            T!["&&"] => (3, 4),
            T!["=="] | T!["!="] => (5, 6),
            T!["<"]
            | T!["<="]
            | T!["<."]
            | T!["<=."]
            | T![">"]
            | T![">="]
            | T![">."]
            | T![">=."] => (7, 8),
            T!["<>"] => (9, 10),
            T!["|>"] => (11, 12),
            T!["+"] | T!["-"] | T!["+."] | T!["-."] => (13, 14),
            T!["*"] | T!["/"] | T!["*."] | T!["/."] | T!["%"] => (15, 16),
            _ => return None,
        })
    }
}
