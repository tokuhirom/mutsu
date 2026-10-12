//! The one predicate shape a `where` clause or a `subset` predicate is
//! answered by without running its bytecode: a single numeric comparison of
//! the bound value against an integer literal (`where * < 100`,
//! `subset Small of Int where * < 100`).
//!
//! Running the predicate's precompiled body costs a nested VM run (register
//! save/restore, `$_` bind and unbind) per check, ~2.4k instructions, for one
//! comparison (#10111). The shape is decided once from the AST, when the
//! predicate is compiled or registered; the runtime evaluator
//! (`Interpreter::topic_int_compare_holds`) answers through the same
//! comparison routine the `NumLt`/`NumLe`/... opcodes run, and declines
//! whenever that routine might not take its plain-Int answer.

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::ValueView;

/// The numeric comparison of a [`TopicIntCompare`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub(crate) enum IntCompareOp {
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
}

impl IntCompareOp {
    /// The operator's canonical routine name, the key a user override of it
    /// is registered under.
    pub(crate) fn infix_name(self) -> &'static str {
        match self {
            IntCompareOp::Lt => "infix:<<>",
            IntCompareOp::Le => "infix:<<=>",
            IntCompareOp::Gt => "infix:<>>",
            IntCompareOp::Ge => "infix:<>=>",
            IntCompareOp::Eq => "infix:<==>",
        }
    }
}

/// `<bound value> <op> <integer literal>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode)]
pub(crate) struct TopicIntCompare {
    pub(crate) op: IntCompareOp,
    pub(crate) rhs: i64,
}

impl TopicIntCompare {
    /// The comparison `body` consists of, when it is exactly one statement
    /// `$<bind> <op> <Int literal>`; `bind` is the variable the predicate's
    /// value is bound to (`_` for a WhateverCode or a bare block).
    // Cost: O(1).
    pub(crate) fn of_body(body: &[Stmt], bind: &str) -> Option<Self> {
        let [
            Stmt::Expr(Expr::Binary {
                left, op, right, ..
            }),
        ] = body
        else {
            return None;
        };
        let op = match op {
            TokenKind::Lt => IntCompareOp::Lt,
            TokenKind::Lte => IntCompareOp::Le,
            TokenKind::Gt => IntCompareOp::Gt,
            TokenKind::Gte => IntCompareOp::Ge,
            TokenKind::EqEq => IntCompareOp::Eq,
            _ => return None,
        };
        let (Expr::Var(name), Expr::Literal(lit)) = (left.as_ref(), right.as_ref()) else {
            return None;
        };
        if name != bind {
            return None;
        }
        match lit.view() {
            ValueView::Int(rhs) => Some(TopicIntCompare { op, rhs }),
            _ => None,
        }
    }
}
