//! The block a slang's `statementlist` wrapper runs in (ADR-16888 §2.6).
//!
//! In rakudo the routine whose body is being parsed is `$*BLOCK`: `method-def`
//! enters a `RakuAST::Method` scope before it parses the body, so a slang's
//! `method statementlist` can ask `$*BLOCK ~~ RakuAST::Method` and read its name
//! and signature (EEL parses a `pattern` body in its own language this way). The
//! Rust parser holds the same facts when it reaches the body; this builds the node
//! the host binds to `$*BLOCK` from them.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::ParamDef;
use crate::scope::NameScope;
use crate::value::Value;

/// The kind of routine whose body a statement list is.
#[derive(Clone, Copy)]
pub(crate) enum RoutineKind {
    Sub,
    Method,
    Submethod,
}

/// `RakuAST::Method` / `Sub` / `Submethod` with the routine's `name` and
/// `signature` (none when the declaration writes none, as in rakudo), the scope
/// node a routine body's statement list is parsed in. The body is not there yet:
/// it is what is being parsed.
// Cost: O(p), p = parameters.
pub(crate) fn routine_scope_node(
    kind: RoutineKind,
    name: &str,
    params: Option<&[ParamDef]>,
) -> Option<Value> {
    let class = match kind {
        RoutineKind::Sub => RakuAstClass::Sub,
        RoutineKind::Method => RakuAstClass::Method,
        RoutineKind::Submethod => RakuAstClass::Submethod,
    };
    let names = NameScope::default();
    let mut fields = Vec::with_capacity(2);
    if !name.is_empty() {
        fields.push(RakuAstField {
            name: Some("name"),
            value: RakuAstFieldValue::Node(Value::rakuast(Box::new(
                super::view::name_from_identifier(name),
            ))),
        });
    }
    // Without a written signature rakudo's scope has none yet (the type object)
    // while the body is parsed, where a node built later shows an empty one.
    let signature = match params {
        Some(params) => Value::rakuast(Box::new(
            super::routine_origins::anon_signature(&names, params, None).ok()?,
        )),
        None => Value::package(crate::symbol::Symbol::intern("RakuAST::Signature")),
    };
    fields.push(RakuAstField {
        name: Some("signature"),
        value: RakuAstFieldValue::Node(signature),
    });
    Some(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })))
}
