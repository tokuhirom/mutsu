//! `Num(EXPR)` / `Hash[Int](...)`: calling a type name.
//!
//! Raku has one node for it, a call on the type term:
//! `ApplyPostfix(Type::Simple, Call::Term(args))` -- the same node `Type.(EXPR)`
//! is (measured on 2026.09). mutsu runs the two differently: a `Type(EXPR)` is
//! the coercion call (`Expr::Call` named after the type) and `Type.(EXPR)` a
//! call on the type object (`Expr::CallOn`), and they differ for a role with a
//! `CALL-ME` and for `Type.()`. The coercion spelling therefore leaves a hidden
//! marker on its `Call::Term`, which the lowering reads back; a hand-built
//! `Call::Term` has none and stays a call on the type object.

use super::convert::{arg_list, node_field};
use super::lower::arg_exprs;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, bareword};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value};

/// The hidden marker field's name.
const FIELD: &str = "type-call";

/// Whether `field` is the hidden marker, which no renderer shows.
// Cost: O(1).
pub(super) fn is_marker(field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

/// `Num(EXPR)` -> the call on the type term; `None` when `name` is not a type
/// name. `args` is absent for `Int()`.
// Cost: O(k + n), k = length of `name`, n = size of the arguments.
pub(super) fn convert(name: &str, args: &[Expr]) -> Result<Option<RakuAstNode>, RuntimeError> {
    if !name.starts_with(char::is_uppercase) {
        return Ok(None);
    }
    let Some(base) = bareword::convert(name) else {
        return Ok(None);
    };
    if base.class != RakuAstClass::TypeSimple {
        return Ok(None);
    }
    let mut call_term = RakuAstNode {
        class: RakuAstClass::CallTerm,
        fields: Vec::new(),
    };
    if !args.is_empty() {
        call_term
            .fields
            .push(node_field(Some("args"), arg_list(args)?));
    }
    call_term.fields.push(RakuAstField {
        name: Some(FIELD),
        value: RakuAstFieldValue::Node(Value::TRUE),
    });
    Ok(Some(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), base),
            node_field(Some("postfix"), call_term),
        ],
    }))
}

/// The coercion call `Type(EXPR)` a marked `Call::Term` on `operand` spells;
/// `None` for an unmarked one or an operand that is not a type name.
// Cost: O(n), n = size of the arguments.
pub(super) fn lower(operand: &Expr, postfix: &RakuAstNode) -> Result<Option<Expr>, RuntimeError> {
    if !postfix.fields.iter().any(is_marker) {
        return Ok(None);
    }
    let Expr::BareWord(type_name) = operand else {
        return Ok(None);
    };
    Ok(Some(Expr::Call {
        name: crate::symbol::Symbol::intern(type_name),
        args: arg_exprs(postfix)?,
        listop: false,
    }))
}
