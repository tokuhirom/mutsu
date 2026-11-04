//! The anonymous variables, `$` / `@` / `%` written without a name.
//!
//! A bare `$` is a `state` variable of the block it appears in. The parser
//! mints a fresh name for each occurrence (`__ANON_STATE#<id>__`, or
//! `__ANON_STATE#PC_<id>__` below a routine body, where the block literal is
//! cloned per call) and declares it as a `state` at the top of the enclosing
//! block (`parser::stmt::simple::take_anon_state_decls`); a bare `@` is the
//! minted `__ANON_ARRAY_<id>__` and a bare `%` the shared `__ANON_HASH__`.
//!
//! Rakudo has a node for each, `VarDeclaration::Anonymous`, and none of the
//! declarations: this module says which names and which statements are the
//! parser's, so the RakuAST converter can render the node and drop the
//! declarations, and the lowering can mint and declare them again.
//!
//! Scalar storage names include `#`, which cannot appear in a source
//! identifier. Runtime state and invocant handling must never capture a legal
//! source name merely because it resembles an old anonymous-state spelling.

use super::{Expr, Stmt};
use crate::value::Value;

const STATE_PREFIX: &str = "__ANON_STATE#";
const STATE_PER_CALL_PREFIX: &str = "__ANON_STATE#PC_";
const ARRAY_PREFIX: &str = "__ANON_ARRAY_";
const HASH_PREFIX: &str = "__ANON_HASH_";

static HASH_COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// A fresh name for a bare `%` (each occurrence is its own anonymous hash).
// Cost: O(1).
pub(crate) fn fresh_hash_name() -> String {
    let id = HASH_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("{HASH_PREFIX}{id}__")
}

fn is_minted(name: &str, prefix: &str) -> bool {
    name.strip_prefix(prefix)
        .and_then(|rest| rest.strip_suffix("__"))
        .is_some_and(|id| !id.is_empty() && id.bytes().all(|b| b.is_ascii_digit()))
}

/// Whether `name` is a minted anonymous scalar (`__ANON_STATE#<id>__`).
// Cost: O(|name|).
pub(crate) fn is_scalar(name: &str) -> bool {
    is_minted(name, STATE_PREFIX) || is_minted(name, STATE_PER_CALL_PREFIX)
}

/// Whether `name` (without its sigil) is a minted anonymous array.
// Cost: O(|name|).
pub(crate) fn is_array(name: &str) -> bool {
    is_minted(name, ARRAY_PREFIX)
}

/// Whether `name` (without its sigil) is an anonymous hash: the shared
/// `__ANON_HASH__` of a bare `%` in a signature or an undeclared position, or
/// a minted `__ANON_HASH_<id>__`.
// Cost: O(|name|).
pub(crate) fn is_hash(name: &str) -> bool {
    name == "__ANON_HASH__" || is_minted(name, HASH_PREFIX)
}

/// The `state` declaration the parser puts at the top of a block for the
/// minted anonymous variable `name` (a scalar's bare name, or an array's /
/// hash's with its sigil, as a `state @x` declaration spells it).
// Cost: O(|name|).
pub(crate) fn implicit_decl(name: String) -> Stmt {
    // The empty container a `state @x;` / `state %h;` starts as.
    let initial = match name.chars().next() {
        Some('@') => Value::real_array(Vec::new()),
        Some('%') => Value::hash(crate::value::ValueMap::default()),
        _ => Value::NIL,
    };
    Stmt::VarDecl {
        marks: Default::default(),
        anonymous: true,
        name,
        expr: Expr::Literal(initial),
        type_constraint: None,
        is_state: true,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer: false,
        constant: None,
        bind: None,
        where_constraint: None,
    }
}

/// Whether `stmt` is such a declaration, which RakuAST does not have.
// Cost: O(|name|).
pub(crate) fn is_implicit_decl(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::VarDecl {
            anonymous: true,
            name,
            expr: Expr::Literal(_),
            is_state: true,
            custom_traits,
            marks,
            ..
        } if custom_traits.is_empty() && marks.is_plain() && is_minted_declaration(name)
    )
}

/// Whether `name` is the declared name of a minted anonymous variable: a
/// scalar's bare name, or an array's / hash's with its sigil.
// Cost: O(|name|).
fn is_minted_declaration(name: &str) -> bool {
    match name.strip_prefix('@') {
        Some(rest) => is_minted(rest, ARRAY_PREFIX),
        None => match name.strip_prefix('%') {
            Some(rest) => is_minted(rest, HASH_PREFIX),
            None => is_scalar(name),
        },
    }
}
