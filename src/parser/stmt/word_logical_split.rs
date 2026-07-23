//! Statement-level word-logical tail wrapping.
//!
//! In Raku the loose word-logicals `and`/`or`/`xor`/`andthen`/`orelse`/
//! `notandthen` are the *loosest* infix operators — looser than item assignment
//! (`=`), the comma operator (`,`), and `return`/statement-level parsing. So
//! `my $x = 1 and 2` is `(my $x = 1) and 2` and `return True and False` is
//! `(return True) and False` (the `return` fires; `and False` is dead code).
//!
//! The hand-rolled statement-level assignment, declaration and `return` parsers
//! parse their RHS with [`expression_no_word_logical`], which stops before a
//! top-level (unparenthesized) word-logical. This module then re-attaches the
//! trailing `... and ...` at the correct, looser precedence: it wraps the
//! value-producing statement in a scopeless [`Stmt::SyntheticBlock`] whose second
//! statement re-reads the assigned variable and applies the word-logical tail.
//!
//! Re-reading the assigned variable (`seed`) as the tail's left operand is exact
//! for `= ` assignment: the variable holds precisely the value the assignment
//! produced, so the boolean / definedness short-circuit and the `andthen`/
//! `orelse` value pass-through all match `(<assignment>) <op> ...`.

use crate::ast::{Expr, Stmt};
use crate::parser::expr::{starts_with_loose_word_logical, word_logical_tail_pub};
use crate::parser::helpers::ws;
use crate::parser::parse_result::PResult;

/// The read expression for an assignment target's variable, used to re-read the
/// just-assigned value as the left operand of the hoisted word-logical.
/// `$x` is stored as `Var("x")`, `@a` as `ArrayVar("a")`, `%h` as `HashVar("h")`.
pub(crate) fn seed_read_expr(name: &str) -> Expr {
    if let Some(n) = name.strip_prefix('@') {
        Expr::ArrayVar(n.to_string())
    } else if let Some(n) = name.strip_prefix('%') {
        Expr::HashVar(n.to_string())
    } else {
        Expr::Var(name.trim_start_matches('$').to_string())
    }
}

/// If `rest` begins with a loose word-logical operator, parse the word-logical
/// tail (seeded by `seed`, a re-read of the just-assigned variable) and wrap
/// `stmt` in a scopeless `SyntheticBlock([stmt, Expr(tail)])`. Otherwise returns
/// `(rest, stmt)` unchanged.
pub(crate) fn wrap_trailing_word_logical<'a>(
    rest: &'a str,
    stmt: Stmt,
    seed: Expr,
) -> PResult<'a, Stmt> {
    let (r, _) = ws(rest)?;
    if !starts_with_loose_word_logical(r) {
        return Ok((rest, stmt));
    }
    let (r, tail) = word_logical_tail_pub(r, seed)?;
    Ok((r, Stmt::SyntheticBlock(vec![stmt, Stmt::Expr(tail)])))
}
