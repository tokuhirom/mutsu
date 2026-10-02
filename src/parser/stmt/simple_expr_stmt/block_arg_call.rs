//! A statement-initial bareword followed by a block: a listop call.

use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::parser::parse_result::PResult;
use crate::parser::primary::ident::make_call_expr;
use crate::parser::primary::misc::{braces_are_hash_composer, parse_block_body};
use crate::parser::stmt::modifier::parse_statement_modifier;

/// `retry { ... }` at the start of a statement, where `retry` is a sub declared
/// later in the compilation unit (Pakku's `sub retry (&code, ...)`, Pod::To::HTML's
/// `debug { ... }`): rakudo parses the bareword as a call to the post-declared
/// routine with the block as its argument, and checks at CHECK time that the
/// routine exists. The expression parser leaves a bareword alone when a lone
/// block follows it — a condition such as `if foo { ... }` must keep its block —
/// so the statement parsers, where no such block is owed, take it here.
///
/// `expr` is what the expression parser produced and `rest` what follows it on
/// the same line. Returns `None` when this is not that shape.
pub(in crate::parser) fn bareword_block_call_expr<'a>(
    input: &'a str,
    expr: &Expr,
    rest: &'a str,
) -> Option<(&'a str, Expr)> {
    let Expr::BareWord(name) = expr else {
        return None;
    };
    // A type name is never a call head: `Foo { ... }` stays the error it is.
    if !name.starts_with(|c: char| c.is_ascii_lowercase() || c == '_') || name.contains("::") {
        return None;
    }
    if !rest.starts_with('{') || braces_are_hash_composer(rest) {
        return None;
    }
    let (r, body) = parse_block_body(rest).ok()?;
    Some((
        r,
        make_call_expr(name.clone(), input, vec![make_anon_sub(body)]),
    ))
}

/// [`bareword_block_call_expr`] as a whole statement, modifiers included.
pub(super) fn bareword_block_call<'a>(
    input: &'a str,
    expr: &Expr,
    rest: &'a str,
) -> Option<PResult<'a, Stmt>> {
    let (r, call) = bareword_block_call_expr(input, expr, rest)?;
    Some(parse_statement_modifier(r, Stmt::Expr(call)))
}
