//! Origins of a type application written in the source (ADR-10723 Stage 2;
//! #15780): `Array[Int]`, `Hash[Str, Array[Int]]`, as a term, a declaration's
//! or a parameter's type, or a routine's return type.
//!
//! Rakudo 2026.11 places the `Type::Parameterized` over the whole application,
//! its base `Type::Simple` over the whole application too (not just the base
//! name), the base's `Name` over the name, the `ArgList` over the text between
//! the brackets (blanks around it excluded), and each argument over its own
//! text.

use super::infix_origins::{byte_range, with_child};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Give the parts of `node`, a `Type::Parameterized` written at `span`, the
/// ranges Rakudo gives them. A part that already has a range keeps it, except
/// the base type, whose range is the application's.
// Cost: O(n + d), n = length of the written application, d = nesting.
pub(super) fn parameterized(node: &mut RakuAstNode, span: SrcSpan) {
    if node.class != RakuAstClass::TypeParameterized || !span.is_some() {
        return;
    }
    let from = span.from as usize;
    let Some(shape) = super::unit_source::with_text(span, application_shape) else {
        return;
    };
    super::origin::set_span(node, span);
    with_child(node, "base-type", |base| {
        if base.class != RakuAstClass::TypeSimple {
            return;
        }
        // The base type spans the whole application; its name only the name.
        base.origin = Default::default();
        super::origin::set_span(base, span);
        with_first(base, |name| {
            name.origin = Default::default();
            super::origin::set_span(name, SrcSpan::new(from, from + shape.name_end));
        });
    });
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("args")) else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(args) = held.view() else {
        return;
    };
    let mut args = RakuAstNode::clone(args);
    if args.class != RakuAstClass::ArgList || args.fields.len() != shape.args.len() {
        return;
    }
    super::origin::set_span(
        &mut args,
        SrcSpan::new(from + shape.inner.0, from + shape.inner.1),
    );
    for (arg, &(a, b)) in args.fields.iter_mut().zip(&shape.args) {
        let RakuAstFieldValue::Node(value) = &mut arg.value else {
            continue;
        };
        let ValueView::RakuAst(inner) = value.view() else {
            continue;
        };
        let mut inner = RakuAstNode::clone(inner);
        let arg_span = SrcSpan::new(from + a, from + b);
        if byte_range(&inner).is_none() {
            super::routine_origins::type_origin(&mut inner, arg_span);
        }
        *value = Value::rakuast(Box::new(inner));
    }
    *held = Value::rakuast(Box::new(args));
}

/// Where the parts of a written application sit, as offsets into its text.
struct Shape {
    /// The end of the base name (the `[`).
    name_end: usize,
    /// The text between the brackets, blanks around it excluded.
    inner: (usize, usize),
    /// Each comma-separated argument, blanks around it excluded.
    args: Vec<(usize, usize)>,
}

/// The shape of `written`, an application `Name[arg, ...]` that ends at its
/// `]`; `None` for any other spelling (quotes, nested parentheses, ...).
// Cost: O(n), n = length of `written`.
fn application_shape(written: &str) -> Option<Shape> {
    let open = written.find('[')?;
    if open == 0 || !written.ends_with(']') {
        return None;
    }
    let close = written.len() - 1;
    let body = &written[open + 1..close];
    let trimmed = |a: usize, b: usize| -> Option<(usize, usize)> {
        let text = &written[a..b];
        let lead = text.len() - text.trim_start().len();
        let trail = text.len() - text.trim_end().len();
        (lead + trail < text.len()).then_some((a + lead, b - trail))
    };
    let inner = trimmed(open + 1, close)?;
    let mut args = Vec::new();
    let mut depth = 0usize;
    let mut start = open + 1;
    for (at, c) in body.char_indices() {
        let at = open + 1 + at;
        match c {
            '[' => depth += 1,
            ']' => depth = depth.checked_sub(1)?,
            '(' | ')' | '{' | '}' | '"' | '\'' | '<' | '>' | '#' => return None,
            ',' if depth == 0 => {
                args.push(trimmed(start, at)?);
                start = at + 1;
            }
            _ => {}
        }
    }
    if depth != 0 {
        return None;
    }
    args.push(trimmed(start, close)?);
    Some(Shape {
        name_end: open,
        inner,
        args,
    })
}

/// Apply `f` to the node in `node`'s first field, if it holds one.
// Cost: O(1) plus `f`.
fn with_first(node: &mut RakuAstNode, f: impl FnOnce(&mut RakuAstNode)) {
    let Some(field) = node.fields.first_mut() else {
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

/// `:16($n)` placed at its range: the `Circumfix::Parentheses` it holds runs
/// from the `(` after the radix to the term's end, and the list inside is
/// placed from the expression's own range.
// Cost: O(n), n = length of the term.
pub(super) fn radix_parentheses(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let open = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let digits = text.strip_prefix(':')?;
        let radix = digits.find(|c: char| !c.is_ascii_digit())?;
        (radix > 0 && digits[radix..].starts_with('(') && text.ends_with(')'))
            .then_some(from + 1 + radix)
    });
    let Some(open) = open else {
        return;
    };
    for field in &mut node.fields {
        let RakuAstFieldValue::Node(held) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(inner) = held.view() else {
            continue;
        };
        if inner.class != RakuAstClass::CircumfixParentheses {
            continue;
        }
        let mut inner = RakuAstNode::clone(inner);
        super::origin::set_span(&mut inner, SrcSpan::new(open, to));
        super::list_origins::circumfix(&mut inner);
        *held = Value::rakuast(Box::new(inner));
    }
}

/// Give the parts of `node`, a `Type::Coercion` written at `span`
/// (`Str(Int)`, `Str()`), the ranges Rakudo gives them: the base type spans
/// the whole coercion and its `Name` the name before the `(`; the constraint
/// spans the text between the parentheses.
// Cost: O(n), n = length of the written coercion.
pub(super) fn coercion(node: &mut RakuAstNode, span: SrcSpan) {
    let from = span.from as usize;
    let Some((name_end, inner)) = super::unit_source::with_text(span, |text| {
        let open = text.find('(')?;
        let body = text[open + 1..].strip_suffix(')')?;
        let name = text[..open].trim_end();
        let lead = body.len() - body.trim_start().len();
        let inner = (!body.trim().is_empty())
            .then(|| (open + 1 + lead, open + 1 + lead + body.trim().len()));
        (!name.is_empty() && !body.contains(['(', ')'])).then_some((name.len(), inner))
    }) else {
        return;
    };
    let place_simple = |base: &mut RakuAstNode, name_end: usize| {
        base.origin = Default::default();
        super::origin::set_span(base, span);
        with_first(base, |name| {
            name.origin = Default::default();
            super::origin::set_span(name, SrcSpan::new(from, from + name_end));
        });
    };
    with_child(node, "base-type", |base| match base.class {
        RakuAstClass::TypeSimple => place_simple(base, name_end),
        // `Int:D(Str)`: Rakudo leaves the `Type::Definedness` unplaced and
        // places its `Type::Simple` as a plain base type's, over the whole
        // coercion, its `Name` over the name without the smiley.
        RakuAstClass::TypeDefinedness => {
            let bare = super::unit_source::with_text(SrcSpan::new(from, from + name_end), |t| {
                let bare = t
                    .strip_suffix(":D")
                    .or_else(|| t.strip_suffix(":U"))
                    .or_else(|| t.strip_suffix(":_"))?;
                Some(bare.len())
            });
            if let Some(bare) = bare {
                base.origin = Default::default();
                with_first(base, |inner| {
                    if inner.class == RakuAstClass::TypeSimple {
                        place_simple(inner, bare)
                    }
                });
            }
        }
        _ => {}
    });
    if let Some((a, b)) = inner {
        with_child(node, "constraint", |constraint| {
            super::routine_origins::type_origin(constraint, SrcSpan::new(from + a, from + b))
        });
    }
}
