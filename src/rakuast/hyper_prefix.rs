//! Hyper prefix operators (`-« @a`, `++<< @a`) in RakuAST, measured on rakudo
//! 2026.09: `ApplyPrefix(prefix => MetaPrefix::Hyper(Prefix("-")), operand)`.
//!
//! The parser builds the typed [`HyperPrefix`] node (ADR-15334 slice 1);
//! [`node`] is its view and [`lower_hyper_prefix`] the one lowering to the call
//! the runtime executes, shared by the parser's node and a hand-built tree.

use super::lower::{lower_expr, named_child, named_child_or_positional, positional_leaf};
use super::view::{leaf_field, node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::ast::parsed::{HyperPrefix, ParsedExpr};
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The routine the lowered hyper prefix calls.
const HYPER_PREFIX: &str = "__mutsu_hyper_prefix";

/// The typed node the parser builds for `SYMBOL« OPERAND`.
// Cost: O(1).
pub(crate) fn hyper_prefix_expr(symbol: impl ToString, operand: Expr) -> Expr {
    Expr::Ast(Box::new(ParsedExpr::HyperPrefix(HyperPrefix {
        symbol: symbol.to_string(),
        operand,
    })))
}

/// The call the runtime executes for a hyper prefix, or `None` when `symbol` is
/// not a prefix the parser itself hypers.
// Cost: O(1).
pub(crate) fn lower_hyper_prefix(symbol: &str, operand: Expr) -> Option<Expr> {
    // The hyper slip `|<<` slips its operand, as the plain `|` does.
    if symbol == "|" {
        return Some(Expr::Unary {
            op: crate::token_kind::TokenKind::Pipe,
            expr: Box::new(operand),
            word: false,
        });
    }
    crate::op_names::prefix_op_name_to_token_kind(symbol)?;
    Some(Expr::Call {
        name: Symbol::intern(HYPER_PREFIX),
        args: vec![Expr::Literal(Value::str(symbol.to_string())), operand],
        listop: false,
    })
}

/// The `ApplyPrefix` of a hyper prefix `symbol` over `operand`.
// Cost: O(n), n = size of the operand.
pub(super) fn node(
    names: &NameScope,
    symbol: &str,
    operand: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let prefix = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Prefix,
        fields: vec![leaf_field(None, Value::str(symbol.to_string()))],
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
            node_field(Some("operand"), view_expr(names, operand)?),
        ],
    })
}

/// The `ApplyPrefix` of a marked hyper slip (`|<< EXPR`): the parser's `|EXPR`.
// Cost: O(n), n = size of the operand.
pub(super) fn slip_node(names: &NameScope, slip: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match slip {
        Expr::Unary { expr, .. } => node(names, "|", expr),
        other => view_expr(names, other),
    }
}

/// The `ApplyPostfix` of a marked hyper call (`@a».(args)`): the parser's
/// `@a».CALL-ME(args)` as `MetaPostfix::Hyper(Call::Term(args))`.
// Cost: O(n), n = size of the target and arguments.
pub(super) fn call_term_node(names: &NameScope, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::HyperMethodCall {
        target,
        args,
        modifier: None,
        ..
    } = call
    else {
        return view_expr(names, call);
    };
    let mut call_term = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallTerm,
        fields: Vec::new(),
    };
    if !args.is_empty() {
        call_term.fields.push(node_field(
            Some("args"),
            super::view::arg_list(names, args)?,
        ));
    }
    let hyper = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::MetaPostfixHyper,
        fields: vec![node_field(None, call_term)],
    };
    super::postfix_grouping::view_node(names, target, hyper)
}

/// The call for an `ApplyPrefix` over a `MetaPrefix::Hyper`, or `None` for any
/// other prefix.
// Cost: O(n), n = size of the operand.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
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
        lower_hyper_prefix(&symbol, lower_expr(names, named_child(node, "operand")?)?)
            .ok_or_else(|| super::lower::unsupported(prefix))
    })())
}
