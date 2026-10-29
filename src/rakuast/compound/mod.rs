//! The one lowering of the compound-assignment family (`OP=`, `.=`, set
//! operators, bracketed metaops), ADR-15334. The parser builds the typed
//! `CompoundAssign` node; everything that expands it into the executable
//! shape lives here, so the parser, the `.AST` view and `EVAL` of a
//! hand-built tree share it.

pub(crate) mod expansion;
pub(crate) mod lower;
pub(crate) mod op;
pub(crate) mod set;

// The names the moved files share (they were children of `parser::stmt::assign`).
use crate::ast::{AssignOp, Expr, Stmt};
use crate::parser::PError;
use crate::rakuast::method_lvalue_roundtrip_assign_expr;
use crate::symbol::Symbol;
use crate::token_kind::{MetaAssignIdentity, TokenKind};
use crate::value::Value;
use std::sync::atomic::{AtomicUsize, Ordering};

/// The counter behind every compiler temporary (`__mutsu_tmp_idx_N`) a
/// compound assignment or `.=` introduces; parser sites that mint the same
/// kind of temporary draw from it too, so the names stay unique.
pub(crate) static TMP_INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

use expansion::*;
use op::*;
use set::*;

/// A compound-lowering failure as the runtime error `EVAL` raises.
// Cost: O(n), n = length of the message.
pub(crate) fn runtime_error(error: PError) -> crate::value::RuntimeError {
    crate::value::RuntimeError::new(error.to_string())
}

/// `lhs OP= rhs` as the compiler runs it, for a consumer that holds the base
/// operator by name (the `*`-curry rewrite): a set operator, a short-circuit
/// scalar form, or the general expansion.
// Cost: O(n), n = size of the target and source.
pub(crate) fn expand_compound_assign_expr(lhs: Expr, op: &str, rhs: Expr) -> Result<Expr, String> {
    let Some(op) = compound_assign_op_from_name(op) else {
        // `%h<a> ∪= $set`: a set operator has no `CompoundAssignOp`.
        return set_op_from_spelling(op)
            .map(|tok| build_set_compound_assign_expr(lhs, tok, rhs))
            .ok_or_else(|| format!("unknown compound operator: {op}"));
    };
    if let Expr::Var(name) = &lhs
        && let Some(short) = short_circuit_compound_assign_expr(name, lhs.clone(), op, rhs.clone())
    {
        return Ok(short);
    }
    build_compound_assign_expr(lhs, op, rhs).map_err(|error| error.to_string())
}
