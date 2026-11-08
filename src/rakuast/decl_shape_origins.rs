//! Origins of the `shape` of a declaration, `my Int %h{Str}` / `my @a[3]`
//! (ADR-10723 Stage 2): the `SemiList` runs over the text inside the brackets,
//! each dimension is a `Statement::Expression`; a key type is a
//! `Type::Simple` over its `Name`, all with the type word's range. The shape is built from the key
//! type's text, so it is placed from the declaration's own range, read back
//! and checked: exact or absent.

use super::infix_origins::{byte_range, child, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the `shape` of `node`, a `VarDeclaration::Simple` whose variable is
/// spelled `variable`, when it is a list of simple key types.
// Cost: O(n), n = length of the declaration's source.
pub(super) fn key_types(node: &mut RakuAstNode, variable: &str) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(shape) = child(node, "shape") else {
        return;
    };
    if shape.class != RakuAstClass::SemiList || shape.origin.0.is_some() {
        return;
    }
    let dims = shape.fields.len();
    let Some(parts) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let after = text.find(variable)? + variable.len();
        let rest = &text[after..];
        let (body, closer) = match rest.as_bytes().first()? {
            b'{' => (&rest[1..], '}'),
            b'[' => (&rest[1..], ']'),
            _ => return None,
        };
        let close = body.find(closer)?;
        let inner = &body[..close];
        let base = from + after + 1;
        let mut parts = Vec::new();
        let mut at = 0;
        for piece in inner.split(';') {
            let trimmed = piece.trim();
            let start = at + (piece.len() - piece.trim_start().len());
            parts.push((base + start, base + start + trimmed.len()));
            at += piece.len() + 1;
        }
        Some(parts)
    }) else {
        return;
    };
    if parts.len() != dims || parts.iter().any(|(a, b)| a == b) {
        return;
    }
    let whole = SrcSpan::new(parts[0].0, parts[parts.len() - 1].1);
    with_child(node, "shape", |shape| {
        super::origin::set_span(shape, whole);
        for (i, (a, b)) in parts.iter().enumerate() {
            let span = SrcSpan::new(*a, *b);
            with_child_at(shape, i, |statement| {
                super::origin::set_span(statement, span);
                // A key type is a `Type::Simple` over a `Name` of the same range;
                // any other dimension (`@a[3]`) keeps the expression's own.
                with_child_at(statement, 0, |ty| {
                    if ty.class != RakuAstClass::TypeSimple {
                        return;
                    }
                    super::origin::set_span(ty, span);
                    with_child_at(ty, 0, |name| super::origin::set_span(name, span));
                });
            });
        }
    });
}
