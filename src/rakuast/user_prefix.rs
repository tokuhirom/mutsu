//! A user-declared prefix operator application (`⥷ <name>` with
//! `sub prefix:<⥷>`) is `ApplyPrefix(prefix => Prefix, operand)` in raku. The
//! parser keeps it as the call of the operator routine, `prefix:<⥷>(<name>)`.

use super::view::{arg_list, leaf_field, name_from_identifier, node_field, unsupported, view_expr};
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

/// Whether the routine `name` is a user candidate of a core prefix operator
/// (`prefix:<!>`), whose operator-syntax application the parser spells
/// [`crate::ast::spelled::Spelling::UserPrefixOperator`].
// Cost: O(k), k = name length.
pub(crate) fn user_prefix_overrides_core(name: &str) -> bool {
    name.strip_prefix("prefix:<")
        .and_then(|s| s.strip_suffix('>'))
        .is_some_and(|symbol| !symbol.is_empty() && is_core_prefix(symbol))
}

/// The `ApplyPrefix` the call of a user candidate of a core prefix operator,
/// written as the operator (`!$x`), stands for: its `Prefix` remembers that
/// the operator routine is a user one, so [`lower`] calls it again.
// Cost: O(n), n = size of the operand.
pub(super) fn view_core_override(
    names: &NameScope,
    call: &Expr,
) -> Option<Result<RakuAstNode, RuntimeError>> {
    let Expr::Call { name, args, .. } = call else {
        return None;
    };
    let symbol = name
        .as_str()
        .strip_prefix("prefix:<")?
        .strip_suffix('>')?
        .to_string();
    let [operand] = args.as_slice() else {
        return None;
    };
    Some(view_expr(names, operand).map(|operand| {
        let mut prefix = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Prefix,
            fields: vec![leaf_field(None, Value::str(symbol))],
        };
        prefix.origin.3 = Some(USER_PREFIX_MARK.into());
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), prefix),
                node_field(Some("operand"), operand),
            ],
        }
    }))
}

/// The origin mark of a `Prefix` node whose operator is a user routine.
const USER_PREFIX_MARK: &str = "user-prefix";

/// Whether the `Prefix` node `prefix` stands for a user operator routine
/// ([`view_core_override`]).
// Cost: O(1).
pub(super) fn is_user_override(prefix: &RakuAstNode) -> bool {
    prefix.origin.3.as_deref() == Some(USER_PREFIX_MARK)
}

/// The tree of a `Spelling::UserPrefixOperator`: the operator application a
/// user candidate of a core prefix keeps, or the call itself when it carries
/// more than the operand (`!$x :adv`), which lowers back to that call.
// Cost: as `view_core_override`.
pub(super) fn view_spelled_core(
    names: &NameScope,
    call: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    view_core_override(names, call).unwrap_or_else(|| view_expr(names, call))
}

/// The tree of a `Spelling::PrefixCall`: `not`/`so` written as a call
/// (`not($x)`), which is a `Call::Name`.
// Cost: O(1) plus viewing the operand.
pub(super) fn view_prefix_call(
    names: &NameScope,
    expr: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Unary { op, expr, .. } = expr else {
        return Err(unsupported("prefix call spelling without its prefix"));
    };
    let name = match op {
        crate::token_kind::TokenKind::Bang => "not",
        crate::token_kind::TokenKind::Question => "so",
        _ => return Err(unsupported("prefix call spelling of an unknown prefix")),
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallName,
        fields: vec![
            node_field(Some("name"), name_from_identifier(name)),
            node_field(
                Some("args"),
                arg_list(names, std::slice::from_ref(&**expr))?,
            ),
        ],
    })
}
