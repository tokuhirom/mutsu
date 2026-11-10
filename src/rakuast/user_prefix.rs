//! A user-declared prefix operator application (`⥷ <name>` with
//! `sub prefix:<⥷>`) is `ApplyPrefix(prefix => Prefix, operand)` in raku. The
//! parser keeps it as the call of the operator routine, `prefix:<⥷>(<name>)`.

use super::view::{leaf_field, node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// The operator symbol of a routine name `prefix:<SYMBOL>`.
// Cost: O(k), k = name length.
fn prefix_symbol(name: &str) -> Option<&str> {
    let inner = name.strip_prefix("prefix:<")?.strip_suffix('>')?;
    (!inner.is_empty() && !is_core_prefix(inner)).then_some(inner)
}

/// Whether the core defines the prefix `symbol`: the name maps to a real
/// token, not to the generic named-operator `Ident` catch-all.
// Cost: O(1).
pub(super) fn is_core_prefix(symbol: &str) -> bool {
    !matches!(
        crate::op_names::prefix_op_name_to_token_kind(symbol),
        None | Some(crate::token_kind::TokenKind::Ident(_))
    ) || matches!(symbol, "so" | "not")
}

/// The `ApplyPrefix` node for the call `name(args)` of a user prefix operator,
/// or `None` for any other call.
// Cost: O(n), n = size of the operand.
pub(super) fn view(
    names: &NameScope,
    name: &str,
    args: &[Expr],
) -> Option<Result<RakuAstNode, RuntimeError>> {
    let symbol = prefix_symbol(name)?;
    let [operand] = args else {
        return None;
    };
    Some(view_expr(names, operand).map(|operand| RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPrefix,
        fields: vec![
            node_field(
                Some("prefix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Prefix,
                    fields: vec![leaf_field(None, Value::str(symbol.to_string()))],
                },
            ),
            node_field(Some("operand"), operand),
        ],
    }))
}

/// The call of the user prefix routine an `ApplyPrefix` over a symbol the core
/// does not define stands for.
// Cost: O(n), n = size of the operand.
pub(super) fn lower(symbol: &str, operand: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern(&format!("prefix:<{symbol}>")),
        args: vec![operand],
        listop: false,
    }
}
