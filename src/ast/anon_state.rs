//! The anonymous variables, `$` / `@` / `%` written without a name.
//!
//! A bare `$` is a `state` variable of the block it appears in. The parser
//! mints a fresh name for each occurrence (`__ANON_STATE_<id>__`, or
//! `__ANON_STATE_PC_<id>__` below a routine body, where the block literal is
//! cloned per call) and declares it as a `state` at the top of the enclosing
//! block (`parser::stmt::simple::take_anon_state_decls`); a bare `@` is the
//! minted `__ANON_ARRAY_<id>__` and a bare `%` the shared `__ANON_HASH__`.
//!
//! Rakudo has a node for each, `VarDeclaration::Anonymous`, and none of the
//! declarations: this module says which names and which statements are the
//! parser's, so the RakuAST converter can render the node and drop the
//! declarations, and the lowering can mint and declare them again.

use super::{Expr, Stmt};
use crate::value::Value;

const STATE_PREFIX: &str = "__ANON_STATE_";
const STATE_PER_CALL_PREFIX: &str = "__ANON_STATE_PC_";
const ARRAY_PREFIX: &str = "__ANON_ARRAY_";

fn is_minted(name: &str, prefix: &str) -> bool {
    name.strip_prefix(prefix)
        .and_then(|rest| rest.strip_suffix("__"))
        .is_some_and(|id| !id.is_empty() && id.bytes().all(|b| b.is_ascii_digit()))
}

/// Whether `name` is a minted anonymous scalar (`__ANON_STATE_<id>__`).
// Cost: O(|name|).
pub(crate) fn is_scalar(name: &str) -> bool {
    is_minted(name, STATE_PREFIX) || is_minted(name, STATE_PER_CALL_PREFIX)
}

/// Whether `name` (without its sigil) is a minted anonymous array.
// Cost: O(|name|).
pub(crate) fn is_array(name: &str) -> bool {
    is_minted(name, ARRAY_PREFIX)
}

/// Whether `name` (without its sigil) is the anonymous hash.
// Cost: O(1).
pub(crate) fn is_hash(name: &str) -> bool {
    name == "__ANON_HASH__"
}

/// The `state` declaration the parser puts at the top of a block for the
/// minted anonymous scalar `name`.
// Cost: O(|name|).
pub(crate) fn implicit_decl(name: String) -> Stmt {
    Stmt::VarDecl {
        name,
        expr: Expr::Literal(Value::NIL),
        type_constraint: None,
        is_state: true,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }
}

/// Whether `stmt` is such a declaration, which RakuAST does not have.
// Cost: O(|name|).
pub(crate) fn is_implicit_decl(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::VarDecl {
            name,
            expr: Expr::Literal(_),
            is_state: true,
            custom_traits,
            ..
        } if custom_traits.is_empty() && is_scalar(name)
    )
}
