//! `RakuAST::Node.literalize` (rakudo's `src/core.c/RakuAST/Literalize.rakumod`):
//! the value of a node that is a constant, or `RakuAST::Node::CannotLiteralize`.
//!
//! A node is a constant when it is a literal, or a core operator applied to
//! constants. [`is_constant`] decides that from the node's shape alone, so
//! nothing that could run user code or touch state is ever evaluated; the
//! value itself comes from running the node through the ordinary `EVAL`
//! path (`Interpreter::eval_rakuast_node`).

use super::lower::rakuast_node_of;
use super::{RakuAstClass as C, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The deepest nesting [`is_constant`] follows before it answers no.
const MAX_DEPTH: usize = 512;

/// The infixes that assign, bind or keep state between evaluations.
const STATEFUL_INFIXES: &[&str] = &[
    "=", ":=", "::=", "ff", "fff", "^ff", "ff^", "^ff^", "^fff", "fff^", "^fff^", "does", ".=",
    "?? !!",
];

/// The prefixes and postfixes that modify their operand or the dynamic state.
const STATEFUL_PREFIXES: &[&str] = &["++", "--", "temp", "let", "|", "\\"];
const STATEFUL_POSTFIXES: &[&str] = &["++", "--"];

/// The terms that name a constant.
const CONSTANT_TERMS: &[&str] = &["pi", "e", "tau", "Inf", "NaN", "i", "True", "False", "Nil"];

/// Whether `node` is a constant.
// Cost: O(n), n = size of the node's subtree.
pub(crate) fn is_constant(node: &RakuAstNode) -> bool {
    constant(node, 0)
}

/// Whether the value of `node` is a `Map` where evaluation answers a `Hash`
/// (rakudo's `{a => 1}` literalizes to a `Map`).
// Cost: O(1).
pub(crate) fn is_hash_composer(node: &RakuAstNode) -> bool {
    node.class == C::CircumfixHashComposer
}

/// The value of a constant as `literalize` answers it: a hash anywhere in it
/// is a `Map` and an array a `List` (rakudo: `{a => {b => 1}}` is
/// `Map.new((:a(Map.new((:b(1))))))`, `{a => [1, {b => 2}]}` holds
/// `(1, Map.new((:b(2))))`).
// Cost: O(n), n = size of the value.
pub(crate) fn literal_value(value: Value) -> Value {
    match value.view() {
        ValueView::Hash(map) => {
            let deep: crate::value::ValueMap = map
                .iter()
                .map(|(k, v)| (k.clone(), literal_value(v.clone().deitemize_element())))
                .collect();
            crate::builtins::map_hash_coerce::to_map(Value::hash(deep)).unwrap_or(value)
        }
        ValueView::Array(items, _) => {
            Value::array(items.iter().cloned().map(literal_value).collect())
        }
        _ => value,
    }
}

fn constant(node: &RakuAstNode, depth: usize) -> bool {
    if depth > MAX_DEPTH {
        return false;
    }
    let all = |nodes: Vec<&RakuAstNode>| nodes.into_iter().all(|child| constant(child, depth + 1));
    match node.class {
        C::IntLiteral
        | C::NumLiteral
        | C::RatLiteral
        | C::ComplexLiteral
        | C::VersionLiteral
        | C::StrLiteral
        | C::ColonPairTrue
        | C::ColonPairFalse
        | C::ColonPairNumber => true,
        C::QuotedString => segments(node).iter().all(|s| s.class == C::StrLiteral),
        C::StatementExpression => {
            // A modifier or a label makes it more than its expression.
            node.fields.iter().all(|f| f.name == Some("expression"))
                && named(node, "expression").is_some_and(|e| constant(e, depth + 1))
        }
        C::StatementList
        | C::StatementSequence
        | C::SemiList
        | C::CircumfixParentheses
        | C::CircumfixArrayComposer
        | C::CircumfixHashComposer => all(children(node)),
        C::FatArrow | C::ColonPairValue => {
            named(node, "value").is_some_and(|v| constant(v, depth + 1))
        }
        C::ApplyInfix => {
            (infix_is_core(node, "infix", C::Infix, STATEFUL_INFIXES)
                // `but` is pure; `does` changes its left operand.
                || infix_is_core(node, "infix", C::Mixin, STATEFUL_INFIXES))
                && named(node, "left").is_some_and(|n| constant(n, depth + 1))
                && named(node, "right").is_some_and(|n| constant(n, depth + 1))
        }
        C::ApplyListInfix => {
            infix_is_core(node, "infix", C::Infix, STATEFUL_INFIXES)
                && list(node, "operands")
                    .iter()
                    .all(|operand| constant(operand, depth + 1))
        }
        C::ApplyPrefix => {
            infix_is_core(node, "prefix", C::Prefix, STATEFUL_PREFIXES)
                && named(node, "operand").is_some_and(|n| constant(n, depth + 1))
        }
        C::ApplyPostfix => {
            infix_is_core(node, "postfix", C::Postfix, STATEFUL_POSTFIXES)
                && named(node, "operand").is_some_and(|n| constant(n, depth + 1))
        }
        C::TermName => name_text(node).is_some_and(|name| {
            CONSTANT_TERMS.contains(&name.as_str())
                || matches!(name.as_str(), "Bool::True" | "Bool::False")
        }),
        C::TermEnum => true,
        C::TypeSimple => true,
        _ => false,
    }
}

/// Whether the operator node in field `field` is a plain operator of class
/// `class` that is not in `stateful`.
fn infix_is_core(node: &RakuAstNode, field: &str, class: C, stateful: &[&str]) -> bool {
    let Some(op) = named(node, field) else {
        return false;
    };
    if op.class != class {
        return false;
    }
    match op.fields.first().map(|f| &f.value) {
        Some(RakuAstFieldValue::Node(v)) => match v.view() {
            ValueView::Str(s) => !stateful.contains(&s.as_str()),
            _ => false,
        },
        _ => false,
    }
}

fn named<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstNode> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => rakuast_node_of(v),
            _ => None,
        })
}

fn list<'a>(node: &'a RakuAstNode, name: &str) -> Vec<&'a RakuAstNode> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map(|f| match &f.value {
            RakuAstFieldValue::List(values) => values.iter().filter_map(rakuast_node_of).collect(),
            _ => Vec::new(),
        })
        .unwrap_or_default()
}

fn segments(node: &RakuAstNode) -> Vec<&RakuAstNode> {
    list(node, "segments")
}

/// Every node-valued child of `node`, in field order.
fn children(node: &RakuAstNode) -> Vec<&RakuAstNode> {
    let mut out = Vec::new();
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) => out.extend(rakuast_node_of(v)),
            RakuAstFieldValue::List(values) => {
                out.extend(values.iter().filter_map(rakuast_node_of));
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
    out
}

/// The spelling of the `Name` of a `Term::Name`: its simple parts joined by
/// `::`. A plain identifier is kept by the parser as one string.
fn name_text(node: &RakuAstNode) -> Option<String> {
    let name = children(node).into_iter().next()?;
    let mut parts = Vec::new();
    for field in &name.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => parts.push(s.to_string()),
                _ => parts.push(super::name_parts::part_identifier(v)?),
            },
            RakuAstFieldValue::List(values) => {
                for value in values {
                    parts.push(super::name_parts::part_identifier(value)?);
                }
            }
            RakuAstFieldValue::Adverb(_) => return None,
        }
    }
    (!parts.is_empty()).then(|| parts.join("::"))
}
