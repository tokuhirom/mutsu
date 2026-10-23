//! Conversion of source-written identifiers, independent of generated helpers.
use super::convert::{call_name, leaf_field, node_field, unsupported};
use super::{RakuAstClass, RakuAstNode, name_parts};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value};

/// Source provenance applies to this identifier only. Arguments still pass
/// through ordinary conversion, including its generated-construct guards.
// Cost: O(n), n = identifier length and converted argument nodes.
pub(super) fn written_identifier(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Var(name) => Ok(written_variable("$", name)),
        Expr::ArrayVar(name) => Ok(written_variable("@", name)),
        Expr::HashVar(name) => Ok(written_variable("%", name)),
        Expr::CodeVar(name) => Ok(written_variable("&", name)),
        Expr::Call { name, args, listop } | Expr::UserRoutineCall { name, args, listop } => {
            named_routine_call(expr, name.as_str(), args, *listop)
        }
        _ => Err(unsupported(
            "source identifier without an identifier expression",
        )),
    }
}

pub(super) fn named_routine_call(
    expr: &Expr,
    name: &str,
    args: &[Expr],
    listop: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let mut node = call_name(name, args, listop)?;
    if matches!(expr, Expr::UserRoutineCall { .. }) {
        node.fields
            .push(super::declared_routines::user_call_field());
    }
    Ok(node)
}

/// A source-written name is never an anonymous parser temporary.
// Cost: O(n), n = identifier length.
pub(super) fn written_variable(sigil: &str, name: &str) -> RakuAstNode {
    // A `$!x` private attribute is a `Var::Attribute` of the whole spelling; a
    // `$.x` public one a `Var::Attribute::Public` whose `name` is the whole
    // spelling too (measured on 2026.09).
    if name.len() > 1 && name.starts_with('!') {
        return RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarAttribute,
            fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
        };
    }
    if name.len() > 1 && name.starts_with('.') {
        return RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarAttributePublic,
            fields: vec![leaf_field(
                Some("name"),
                Value::str(format!("{sigil}{name}")),
            )],
        };
    }
    if name.len() > 1 && name.starts_with('*') {
        return RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarDynamic,
            fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
        };
    }
    if name_parts::is_qualified_identifier(name) {
        return RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::VarPackage,
            fields: vec![
                node_field(Some("name"), name_parts::qualified_name(name)),
                leaf_field(Some("sigil"), Value::str(sigil.to_string())),
            ],
        };
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarLexical,
        fields: vec![leaf_field(None, Value::str(format!("{sigil}{name}")))],
    }
}
