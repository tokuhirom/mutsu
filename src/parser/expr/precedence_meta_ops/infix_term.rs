//! `[&TERM]` used as an infix, for a `&`-term that is not a plain name:
//! `1 [&($f)] 2`, `1 [&infix:<+>] 2`, `1 R[&($f)] 2`.
//!
//! rakudo's `infixish` accepts `[&` followed by any `&`-term and calls it with
//! the two operands. A plain `[&name]` keeps its own `Expr::InfixFunc` path
//! (`parse_infix_func_op`); this one covers every other spelling by lowering
//! the operator to an ordinary call on the callable the term evaluates to.

use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::symbol::Symbol;

/// A parsed `[&TERM]` operator: its meta prefix (`R`, `X`, `Z`), the callable
/// term, and the byte length of the whole operator.
pub(crate) struct InfixTermOp {
    pub(crate) modifier: Option<char>,
    pub(crate) callable: Expr,
    pub(crate) len: usize,
}

pub(crate) fn parse_infix_term_op(input: &str) -> Option<InfixTermOp> {
    // A block's `}` ending the previous line ended the statement: an
    // operator spelled here starts a new one (`parser::stmt_ending_brace`).
    if crate::parser::stmt_ending_brace::infix_barred_by_stmt_ending_brace(input) {
        return None;
    }
    let (modifier, rest) = match input.as_bytes().first()? {
        m @ (b'R' | b'X' | b'Z') => (Some(*m as char), &input[1..]),
        _ => (None, input),
    };
    let term_src = rest.strip_prefix('[')?;
    if !term_src.starts_with('&') {
        return None;
    }
    let (r, callable) = crate::parser::primary::var::code_var(term_src).ok()?;
    let (r, _) = ws(r).ok()?;
    let r = r.strip_prefix(']')?;
    Some(InfixTermOp {
        modifier,
        callable,
        len: input.len() - r.len(),
    })
}

/// The call the operator stands for, given its left operand and the right
/// operand(s) (a comma list for `X`, one operand otherwise).
pub(crate) fn infix_term_call(op: InfixTermOp, left: Expr, right: Vec<Expr>) -> Expr {
    match op.modifier {
        // `X` / `Z` over a callable are `cross` / `zip` with `:with`.
        Some(m @ ('X' | 'Z')) => {
            let mut args = vec![left];
            args.extend(right);
            args.push(Expr::Binary {
                left: Box::new(Expr::Literal(crate::value::Value::str_from("with"))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(op.callable),
            });
            Expr::Call {
                name: Symbol::intern(if m == 'X' { "cross" } else { "zip" }),
                args,
            }
        }
        modifier => {
            let mut args = vec![left];
            args.extend(right);
            if modifier == Some('R') {
                args.reverse();
            }
            Expr::CallOn {
                target: Box::new(op.callable),
                args,
            }
        }
    }
}
