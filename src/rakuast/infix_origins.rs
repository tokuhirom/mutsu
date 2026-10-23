//! Origins derived from the origins of a node's operands (ADR-10723 Stage 2,
//! expression families): an `ApplyInfix` and its operator, the `Name` and
//! `ArgList` of a named call, and a variable's `Initializer`.
//!
//! The parser notes terms, not the operators between them, so these nodes are
//! placed from their converted children: an infix application spans its
//! operands, and its operator is the text between them. Every range is
//! checked against the source text it claims (the operator written between
//! the operands is the node's operator, a call's text starts with its name),
//! so a node the parser made rather than read — `$x += 1`'s copy of `$x + 1`
//! — fails the check and stays without an origin: exact or absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Place `node`, a node just converted, and its operator / name / argument
/// parts, from the origins its children carry.
// Cost: O(f + n), f = fields of `node`, n = length of the source text read.
pub(super) fn derive(node: &mut RakuAstNode) {
    match node.class {
        RakuAstClass::ApplyInfix => apply_infix(node),
        RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses => call_name(node),
        RakuAstClass::VarDeclarationSimple => initializer(node),
        RakuAstClass::VarDeclarationSignature => {
            initializer(node);
            super::signature_decl_origins::declaration(node);
        }
        RakuAstClass::ApplyListInfix => super::list_origins::apply_list_infix(node),
        RakuAstClass::CircumfixParentheses | RakuAstClass::CircumfixArrayComposer => {
            super::list_origins::circumfix(node)
        }
        RakuAstClass::ApplyPostfix => {
            super::list_origins::postcircumfix(node);
            super::operator_origins::derive(node);
        }
        RakuAstClass::QuotedString => super::string_origins::interpolated(node),
        RakuAstClass::Block
        | RakuAstClass::Sub
        | RakuAstClass::PointyBlock
        | RakuAstClass::StatementPrefixDo
        | RakuAstClass::StatementPrefixTry
        | RakuAstClass::StatementPrefixGather
        | RakuAstClass::StatementPrefixStart
        | RakuAstClass::StatementPrefixQuietly
        | RakuAstClass::StatementPrefixSink
        | RakuAstClass::StatementPrefixLazy
        | RakuAstClass::StatementPrefixHyper
        | RakuAstClass::StatementPrefixRace
        | RakuAstClass::StatementPrefixEager
        | RakuAstClass::StatementPrefixOnce
        | RakuAstClass::StatementPrefixReact
        | RakuAstClass::StatementPrefixSupply => super::expr_block_origins::derive(node),
        _ => super::operator_origins::derive(node),
    }
}

/// The byte range a node's origin covers.
// Cost: O(1).
pub(super) fn byte_range(node: &RakuAstNode) -> Option<(usize, usize)> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let map = attributes.as_map();
    let at = |name: &str| match map.get(name)?.view() {
        ValueView::Int(char) => super::unit_source::byte_of(char).map(|b| b as usize),
        _ => None,
    };
    Some((at("from")?, at("to")?))
}

/// The node held in `node`'s field `name`.
// Cost: O(f), f = fields of `node`.
pub(super) fn child<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstNode> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
            ValueView::RakuAst(held) => Some(held),
            _ => None,
        },
        _ => None,
    })
}

/// Apply `f` to the node held in `node`'s field `name`, if it holds one.
// Cost: O(f), f = fields of `node`, plus `f`.
pub(super) fn with_child(node: &mut RakuAstNode, name: &str, f: impl FnOnce(&mut RakuAstNode)) {
    let Some(field) = node
        .fields
        .iter_mut()
        .find(|field| field.name == Some(name))
    else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(inner) = held.view() else {
        return;
    };
    let mut inner = RakuAstNode::clone(inner);
    f(&mut inner);
    *held = Value::rakuast(Box::new(inner));
}

/// The text of the first leaf field of `node` (an `Infix`'s operator, a
/// `Name`'s identifier).
// Cost: O(f), f = fields of `node`.
pub(super) fn leaf_text(node: &RakuAstNode) -> Option<String> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })
}

/// How an infix operator node is written: its text, and the range of its
/// inner `Infix` within that text (`R-` holds `-` at 1..2).
// Cost: O(d), d = nesting of metaoperators.
fn written(infix: &RakuAstNode) -> Option<(String, Option<(usize, usize)>)> {
    let inner = || child_at(infix, 0).and_then(leaf_text_of_infix);
    Some(match infix.class {
        RakuAstClass::Infix => (leaf_text(infix)?, None),
        RakuAstClass::Assignment => ("=".to_string(), None),
        RakuAstClass::MetaInfixAssign => {
            let op = inner()?;
            let len = op.len();
            (format!("{op}="), Some((0, len)))
        }
        RakuAstClass::MetaInfixReverse | RakuAstClass::MetaInfixNegate => {
            let op = inner()?;
            let prefix = if infix.class == RakuAstClass::MetaInfixReverse {
                "R"
            } else {
                "!"
            };
            let len = op.len();
            (format!("{prefix}{op}"), Some((1, 1 + len)))
        }
        _ => return None,
    })
}

/// The node in `node`'s `index`-th field.
// Cost: O(1).
pub(super) fn child_at(node: &RakuAstNode, index: usize) -> Option<&RakuAstNode> {
    match &node.fields.get(index)?.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(held) => Some(held),
            _ => None,
        },
        _ => None,
    }
}

// Cost: O(f), f = fields of `node`.
fn leaf_text_of_infix(node: &RakuAstNode) -> Option<String> {
    (node.class == RakuAstClass::Infix)
        .then(|| leaf_text(node))
        .flatten()
}

/// `left OP right`: the application spans its operands (when it has no
/// origin of its own yet), and its operator is the `OP` written between them.
// Cost: O(n), n = length of the source between the operands.
fn apply_infix(node: &mut RakuAstNode) {
    leading_variable(node);
    let left = child(node, "left").and_then(byte_range);
    let right = child(node, "right").and_then(byte_range);
    let Some((text, inner)) = child(node, "infix").and_then(written) else {
        return;
    };
    let own = byte_range(node);
    // The operator follows the left operand after whitespace, or ends where
    // whitespace before the right operand begins.
    let op_at = match (left, right, own) {
        (Some((_, left_end)), Some((right_start, _)), _) if left_end <= right_start => {
            super::unit_source::with_text(SrcSpan::new(left_end, right_start), |between| {
                let after = between.trim_start();
                let rest = after.strip_prefix(text.as_str())?;
                (rest.is_empty() || rest.starts_with(char::is_whitespace) || rest.starts_with('#'))
                    .then_some(left_end + (between.len() - after.len()))
            })
        }
        (None, Some((right_start, _)), Some((own_start, _))) if own_start < right_start => {
            super::unit_source::with_text(SrcSpan::new(own_start, right_start), |before| {
                let trimmed = before.trim_end();
                let head = trimmed.strip_suffix(text.as_str())?;
                (head.is_empty() || head.ends_with(char::is_whitespace))
                    .then_some(own_start + trimmed.len() - text.len())
            })
        }
        _ => None,
    };
    let Some(op_at) = op_at else {
        return;
    };
    if own.is_none()
        && let (Some((from, _)), Some((_, to))) = (left, right)
    {
        super::origin::set_span(node, SrcSpan::new(from, to));
    }
    with_child(node, "infix", |infix| {
        super::origin::set_span(infix, SrcSpan::new(op_at, op_at + text.len()));
        if let Some((from, to)) = inner {
            with_child_at(infix, 0, |op| {
                super::origin::set_span(op, SrcSpan::new(op_at + from, op_at + to))
            });
        }
    });
}

/// The variable an infix application placed at the statement level starts
/// with (`$x = 1`, `$x += 1`): an assignment keeps its target by name, so the
/// parser's note for it finds no term to attach to. Its range is the
/// variable's spelling at the start of the application, when it is written
/// there.
// Cost: O(v), v = length of the variable's name.
fn leading_variable(node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(left) = child(node, "left") else {
        return;
    };
    if left.origin.0.is_some()
        || !matches!(
            left.class,
            RakuAstClass::VarLexical | RakuAstClass::VarDynamic | RakuAstClass::VarAttribute
        )
    {
        return;
    }
    let Some(name) = leaf_text(left) else {
        return;
    };
    let written =
        super::unit_source::with_text(SrcSpan::new(from, from + name.len() + 1), |text| {
            let rest = text.strip_prefix(name.as_str())?;
            (!rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'')))
                .then_some(())
        })
        .or_else(|| {
            // The variable ends the source.
            super::unit_source::with_text(SrcSpan::new(from, from + name.len()), |text| {
                (text == name).then_some(())
            })
        });
    if written.is_some() {
        with_child(node, "left", |left| {
            super::origin::set_span(left, SrcSpan::new(from, from + name.len()))
        });
    }
}

/// Apply `f` to the node in `node`'s `index`-th field.
// Cost: O(1) plus `f`.
pub(super) fn with_child_at(
    node: &mut RakuAstNode,
    index: usize,
    f: impl FnOnce(&mut RakuAstNode),
) {
    let Some(field) = node.fields.get_mut(index) else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(inner) = held.view() else {
        return;
    };
    let mut inner = RakuAstNode::clone(inner);
    f(&mut inner);
    *held = Value::rakuast(Box::new(inner));
}

/// `name(args)` / `name args`: the `Name` is the identifier the call starts
/// with, the `ArgList` the inside of its parentheses, or the range from its
/// first argument to its last.
// Cost: O(n + a), n = length of the call's source, a = its arguments.
fn call_name(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(name) = child(node, "name").and_then(leaf_text) else {
        return;
    };
    let parens = node.class == RakuAstClass::CallName;
    let fits = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        let ok = if parens {
            rest.starts_with('(') && rest.ends_with(')')
        } else {
            rest.is_empty() || rest.starts_with(char::is_whitespace)
        };
        ok.then_some(())
    });
    if fits.is_none() {
        return;
    }
    let name_end = from + name.len();
    let args = if parens {
        Some((name_end + 1, to - 1))
    } else {
        child(node, "args").and_then(|list| {
            let first = child_at(list, 0).and_then(byte_range)?;
            let last = child_at(list, list.fields.len().checked_sub(1)?).and_then(byte_range)?;
            let gap = super::unit_source::with_text(SrcSpan::new(name_end, first.0), |gap| {
                gap.chars().all(char::is_whitespace).then_some(())
            });
            (gap.is_some() && last.1 == to).then_some((first.0, last.1))
        })
    };
    with_child(node, "name", |n| {
        super::origin::set_span(n, SrcSpan::new(from, name_end))
    });
    if let Some((args_from, args_to)) = args {
        with_child(node, "args", |list| {
            super::origin::set_span(list, SrcSpan::new(args_from, args_to))
        });
    }
}

/// `my $x = EXPR` / `my $x := EXPR`: the `Initializer` runs from its `=` /
/// `:=` to the end of the expression.
// Cost: O(n), n = length of the declaration's source.
fn initializer(node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(init) = child(node, "initializer") else {
        return;
    };
    let op = match init.class {
        RakuAstClass::InitializerAssign => "=",
        RakuAstClass::InitializerBind => ":=",
        _ => return,
    };
    let Some((expr_from, expr_to)) = child_at(init, 0).and_then(byte_range) else {
        return;
    };
    if expr_from <= from {
        return;
    }
    let op_at = super::unit_source::with_text(SrcSpan::new(from, expr_from), |before| {
        let head = before.trim_end().strip_suffix(op)?;
        // `==`, `::=` and the like are other operators.
        (!head.ends_with(['=', ':', '!', '<', '>', '~'])).then_some(from + head.len())
    });
    if let Some(op_at) = op_at {
        with_child(node, "initializer", |init| {
            super::origin::set_span(init, SrcSpan::new(op_at, expr_to))
        });
    }
}
