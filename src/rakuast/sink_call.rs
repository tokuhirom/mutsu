//! `needs-sink-call`, as rakudo 2026.09 answers it. `RakuAST::Node` says `True`;
//! the classes below override it (measured with `.^lookup('needs-sink-call')`
//! over every `RakuAST::` class and with `.needs-sink-call` over parsed trees):
//!
//! - declarations and the nodes that are never sunk as a call answer `False`
//!   (packages, routines, regex thunks, `Statement::Expression`, loops,
//!   `Ternary`, `VarDeclaration::*`, `ApplyDottyInfix`, ...);
//! - `Call::Name` answers the `Int` `1`;
//! - `ApplyInfix` looks through `Hyper`/`Reverse` to the operator it applies:
//!   an assignment, `:=`, `~~` or `does` (and every meta-assignment) answer the
//!   `Int` `0`, `xx` answers the `Int` `1`, any other operator `True`.
//!
//! The `Int` answers are rakudo's own types and are kept.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode, hierarchy};
use crate::value::{Value, ValueView};

/// Classes (or roles) whose `needs-sink-call` is `False`.
const FALSE_FOR: &[&str] = &[
    "RakuAST::ApplyDottyInfix",
    "RakuAST::Nqp",
    "RakuAST::Package",
    "RakuAST::RegexThunk",
    "RakuAST::Routine",
    "RakuAST::Statement::Expression",
    "RakuAST::Statement::Loop",
    "RakuAST::Statement::Require",
    "RakuAST::Ternary",
    "RakuAST::Type::Enum",
    "RakuAST::VarDeclaration::Constant",
    "RakuAST::VarDeclaration::Signature",
    "RakuAST::VarDeclaration::Simple",
    "RakuAST::VarDeclaration::Term",
];

/// The first child of `node` when it is a node (the operator a meta operator
/// wraps).
fn first_node(node: &RakuAstNode) -> Option<&RakuAstNode> {
    match node.fields.first().map(|f| &f.value) {
        Some(RakuAstFieldValue::Node(v)) => match v.view() {
            ValueView::RakuAst(inner) => Some(inner),
            _ => None,
        },
        _ => None,
    }
}

/// `needs-sink-call` of the operator `op` an `ApplyInfix` applies: `Hyper` and
/// `Reverse` answer for the operator they wrap.
// Cost: O(d), d = nesting depth of meta operators.
fn infix_answer(op: &RakuAstNode) -> Value {
    match op.class {
        RakuAstClass::Assignment | RakuAstClass::MetaInfixAssign => Value::int(0),
        RakuAstClass::MetaInfixHyper | RakuAstClass::MetaInfixReverse => {
            first_node(op).map_or_else(|| Value::truth(true), infix_answer)
        }
        _ if hierarchy::does(op.class.printed_name(), "RakuAST::Infix") == Some(true) => {
            let text = op.fields.first().and_then(|f| match &f.value {
                RakuAstFieldValue::Node(v) => match v.view() {
                    ValueView::Str(s) => Some(s.to_string()),
                    _ => None,
                },
                _ => None,
            });
            match text.as_deref() {
                Some(":=" | "~~" | "does") => Value::int(0),
                Some("xx") => Value::int(1),
                _ => Value::truth(true),
            }
        }
        _ => Value::truth(true),
    }
}

/// `needs-sink-call` of `node`.
// Cost: O(d + k), d = depth of the class's MRO, k = number of overriding classes.
pub(crate) fn needs_sink_call(node: &RakuAstNode) -> Value {
    let class = node.class.printed_name();
    if hierarchy::does(class, "RakuAST::Call::Name") == Some(true) {
        return Value::int(1);
    }
    if class == "RakuAST::ApplyInfix" {
        let infix = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
            (Some("infix"), RakuAstFieldValue::Node(v)) => Some(v),
            _ => None,
        });
        return match infix.map(|v| v.view()) {
            Some(ValueView::RakuAst(op)) => infix_answer(op),
            _ => Value::truth(true),
        };
    }
    if FALSE_FOR
        .iter()
        .any(|owner| hierarchy::does(class, owner) == Some(true))
    {
        return Value::truth(false);
    }
    Value::truth(true)
}
