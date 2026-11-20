//! `is-pure` and `short-circuit` of the operator nodes, as rakudo 2026.11
//! answers them (measured with the bare `raku` over every built-in operator
//! symbol and the metaoperators):
//!
//! - an `Infix` is pure unless it is one of the side-effecting symbols below
//!   (`does`, `xx`, `~~`, `:=` and the atomic assignments); a `Prefix` unless
//!   it is an increment or decrement; `Assignment` and `MetaInfix::Assign` never;
//!   the other metaoperators (`Negate`, `Reverse`, `Cross`, `Zip`, `Hyper`) and the
//!   function and bracketed infixes (`Infixish`'s default) always;
//! - only the `Infix`es that skip their right operand short-circuit.

use super::operator_props::symbol_text;
use super::{RakuAstClass, RakuAstNode};

/// Infix symbols with a side effect: not pure.
const IMPURE_INFIX: &[&str] = &["does", "xx", "~~", ":=", "⚛=", "⚛+=", "⚛-=", "⚛−="];

/// Prefix symbols with a side effect: not pure.
const IMPURE_PREFIX: &[&str] = &["++", "--", "++⚛", "--⚛"];

/// Infix symbols that may skip evaluating their right operand.
const SHORT_CIRCUIT: &[&str] = &[
    "&&",
    "||",
    "//",
    "and",
    "or",
    "andthen",
    "orelse",
    "notandthen",
];

/// `is-pure` of `node`, or `None` when it is not an operator node.
// Cost: O(f + k), f = fields of the node, k = a constant-size symbol list.
pub(crate) fn is_pure(node: &RakuAstNode) -> Option<bool> {
    Some(match node.class {
        RakuAstClass::Infix => !IMPURE_INFIX.contains(&symbol_text(node)?.as_str()),
        RakuAstClass::Prefix => !IMPURE_PREFIX.contains(&symbol_text(node)?.as_str()),
        RakuAstClass::Assignment | RakuAstClass::MetaInfixAssign => false,
        RakuAstClass::MetaInfixNegate
        | RakuAstClass::MetaInfixReverse
        | RakuAstClass::MetaInfixCross
        | RakuAstClass::MetaInfixZip
        | RakuAstClass::MetaInfixHyper
        | RakuAstClass::FunctionInfix
        | RakuAstClass::BracketedInfix => true,
        _ => return None,
    })
}

/// `short-circuit` of `node`, or `None` when it is not an infix node.
// Cost: O(f + k), f = fields of the node, k = a constant-size symbol list.
pub(crate) fn short_circuit(node: &RakuAstNode) -> Option<bool> {
    Some(match node.class {
        RakuAstClass::Infix => SHORT_CIRCUIT.contains(&symbol_text(node)?.as_str()),
        RakuAstClass::Assignment
        | RakuAstClass::MetaInfixAssign
        | RakuAstClass::MetaInfixNegate
        | RakuAstClass::MetaInfixReverse
        | RakuAstClass::MetaInfixCross
        | RakuAstClass::MetaInfixZip
        | RakuAstClass::MetaInfixHyper
        | RakuAstClass::FunctionInfix
        | RakuAstClass::BracketedInfix => false,
        _ => return None,
    })
}
