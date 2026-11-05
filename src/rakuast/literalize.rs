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
use crate::value::Value;
use crate::value::ValueView;

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

/// The expressions of a block's statements when every statement is a plain
/// expression statement (`{ 1, 2 }`, `-> $x { 1; 2 }`): what rakudo's
/// `Block.literalize` turns into a `Map`. `None` for any other block shape.
// Cost: O(n), n = size of the block's body.
pub(crate) fn block_expressions(node: &RakuAstNode) -> Option<Vec<&RakuAstNode>> {
    if !matches!(node.class, C::Block | C::PointyBlock) {
        return None;
    }
    fn walk<'a>(node: &'a RakuAstNode, out: &mut Vec<&'a RakuAstNode>) -> bool {
        match node.class {
            C::Block | C::PointyBlock | C::Blockoid | C::StatementList | C::StatementSequence => {
                children(node).into_iter().all(|child| match child.class {
                    C::StatementExpression => {
                        if !node_has_only_expression(child) {
                            return false;
                        }
                        named(child, "expression").is_some_and(|e| {
                            out.push(e);
                            true
                        })
                    }
                    C::Blockoid | C::StatementList | C::StatementSequence => walk(child, out),
                    // The parameter list of a pointy block.
                    C::Signature | C::Parameter => true,
                    _ => false,
                })
            }
            _ => false,
        }
    }
    let mut out = Vec::new();
    walk(node, &mut out).then_some(out)
}

fn node_has_only_expression(node: &RakuAstNode) -> bool {
    node.fields.iter().all(|f| f.name == Some("expression"))
}

/// How a constant node is a list of values to literalize one by one.
pub(crate) enum ListShape {
    /// A comma list, or an empty `[]`: the operands (none for `[]`).
    Items(Vec<Value>),
    /// `[ EXPR ]`: the literal of the one expression inside.
    Single(Value),
    /// Anything else.
    Other,
}

// Cost: O(f), f = fields of the node.
fn node_children(node: &RakuAstNode) -> Vec<Value> {
    node.fields
        .iter()
        .flat_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if matches!(v.view(), ValueView::RakuAst(_)) => {
                vec![v.clone()]
            }
            RakuAstFieldValue::List(items) => items.clone(),
            _ => Vec::new(),
        })
        .collect()
}

fn class_of(value: &Value) -> Option<C> {
    match value.view() {
        ValueView::RakuAst(n) => Some(n.class),
        _ => None,
    }
}

/// The list shape of `node`: an `ArrayComposer` over one statement is that
/// statement's expression, over none an empty list; an `ApplyListInfix` of
/// `,` and a `Parentheses` around one are their operands.
// Cost: O(f), f = fields on the way down to the expression.
pub(crate) fn list_shape(node: &RakuAstNode) -> ListShape {
    let statements_of = |value: &Value| -> Vec<Value> {
        match value.view() {
            ValueView::RakuAst(semi) if semi.class == C::SemiList => node_children(semi),
            _ => Vec::new(),
        }
    };
    let expression_of = |statement: &Value| -> Option<Value> {
        let ValueView::RakuAst(st) = statement.view() else {
            return None;
        };
        (st.class == C::StatementExpression)
            .then(|| node_children(st).into_iter().next())
            .flatten()
    };
    match node.class {
        C::CircumfixArrayComposer | C::CircumfixParentheses => {
            let Some(semi) = node_children(node).into_iter().next() else {
                return ListShape::Other;
            };
            let statements = statements_of(&semi);
            match statements.as_slice() {
                [] if node.class == C::CircumfixArrayComposer => ListShape::Items(Vec::new()),
                [one] => match expression_of(one) {
                    Some(expr) if node.class == C::CircumfixArrayComposer => {
                        ListShape::Single(expr)
                    }
                    // `(a, b)` is its comma list; `(a)` is just `a`.
                    Some(expr) if class_of(&expr) == Some(C::ApplyListInfix) => {
                        ListShape::Single(expr)
                    }
                    _ => ListShape::Other,
                },
                _ => ListShape::Other,
            }
        }
        C::ApplyListInfix => {
            let is_comma = node.fields.iter().any(|f| {
                f.name == Some("infix")
                    && matches!(&f.value, RakuAstFieldValue::Node(v)
                        if matches!(v.view(), ValueView::RakuAst(i)
                            if i.fields.iter().any(|g| matches!(&g.value,
                                RakuAstFieldValue::Node(s) if s.as_str() == Some(",")))))
            });
            if !is_comma {
                return ListShape::Other;
            }
            let operands = node
                .fields
                .iter()
                .find_map(|f| match &f.value {
                    RakuAstFieldValue::List(items) if f.name == Some("operands") => {
                        Some(items.clone())
                    }
                    _ => None,
                })
                .unwrap_or_default();
            ListShape::Items(operands)
        }
        _ => ListShape::Other,
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
