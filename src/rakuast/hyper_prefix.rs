//! Hyper prefix operators (`-« @a`, `++<< @a`) in RakuAST, measured on rakudo
//! 2026.09: `ApplyPrefix(prefix => MetaPrefix::Hyper(Prefix("-")), operand)`.
//!
//! The parser spells the operation as the call `__mutsu_hyper_prefix("-", arg)`
//! that the runtime executes; that call is the lowered form both directions
//! map to.

use super::convert::{convert_expr, leaf_field, node_field};
use super::lower::{lower_expr, named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The routine the parser's hyper prefix calls.
const HYPER_PREFIX: &str = "__mutsu_hyper_prefix";

/// The `ApplyPrefix` the call `name(args)` is, or `None` when it is not a
/// hyper prefix.
// Cost: O(n), n = size of the operand.
pub(super) fn convert(name: &str, args: &[Expr]) -> Option<Result<RakuAstNode, RuntimeError>> {
    if name != HYPER_PREFIX {
        return None;
    }
    let [Expr::Literal(symbol), operand] = args else {
        return None;
    };
    let symbol = symbol.as_str()?.to_string();
    Some((|| {
        let prefix = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Prefix,
            fields: vec![leaf_field(None, Value::str(symbol))],
        };
        Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(
                    Some("prefix"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::MetaPrefixHyper,
                        fields: vec![node_field(None, prefix)],
                    },
                ),
                node_field(Some("operand"), convert_expr(operand)?),
            ],
        })
    })())
}

/// The parser's call for an `ApplyPrefix` over a `MetaPrefix::Hyper`, or
/// `None` for any other prefix.
// Cost: O(n), n = size of the operand.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    let prefix = named_child(node, "prefix").ok()?;
    if prefix.class != RakuAstClass::MetaPrefixHyper {
        return None;
    }
    Some((|| {
        let op = named_child_or_positional(prefix)?;
        let symbol = match positional_leaf(op)?.view() {
            ValueView::Str(s) if op.class == RakuAstClass::Prefix => s.to_string(),
            _ => return Err(super::lower::unsupported(prefix)),
        };
        // Only the prefixes the parser itself hypers have a runtime form.
        if crate::compiler::helpers_ops::prefix_op_name_to_token_kind(&symbol).is_none() {
            return Err(super::lower::unsupported(prefix));
        }
        Ok(Expr::Call {
            name: Symbol::intern(HYPER_PREFIX),
            args: vec![
                Expr::Literal(Value::str(symbol)),
                lower_expr(named_child(node, "operand")?)?,
            ],
            listop: false,
        })
    })())
}
