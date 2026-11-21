//! Origins of a sigilless declaration (`my \x = 1`, `our \T := Int`;
//! ADR-10723 Stage 2, #15780): its `Name` is the identifier after the
//! backslash.

use super::RakuAstNode;
use crate::ast::SrcSpan;

/// Place the `Name` of `node`, a `VarDeclaration::Term` with a range, over the
/// identifier written after its `\`, and its type over the type written
/// between the declarator and the `\`.
// Cost: O(n), n = length of the declaration's source up to the name.
pub(super) fn name(node: &mut RakuAstNode) {
    let Some((from, to)) = super::infix_origins::byte_range(node) else {
        return;
    };
    let Some(text) = super::infix_origins::child(node, "name")
        .and_then(|name| match super::name_parts::name_shape(name)? {
            super::name_parts::NameShape::Identifier(text) => Some(text),
            _ => None,
        })
    else {
        return;
    };
    let at = super::unit_source::with_text(SrcSpan::new(from, to), |source| {
        let slash = source.find('\\')?;
        let after = &source[slash + 1..];
        let lead = after.len() - after.trim_start().len();
        let rest = &after[lead..];
        (rest.starts_with(text.as_str())
            && !rest[text.len()..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-'))
        .then_some(from + slash + 1 + lead)
    });
    if let Some(at) = at {
        super::infix_origins::with_child(node, "name", |name| {
            super::origin::set_span(name, SrcSpan::new(at, at + text.len()))
        });
    }
    // `my Int \n`: the type between the declarator and the backslash.
    let ty = super::unit_source::with_text(SrcSpan::new(from, to), |source| {
        let head = source[..source.find('\\')?].trim_end();
        let (_, rest) = head.split_once(char::is_whitespace)?;
        let ty = rest.trim_start();
        let at = from + head.len() - ty.len();
        (!ty.is_empty()).then(|| SrcSpan::new(at, at + ty.len()))
    });
    if let Some(ty) = ty {
        super::infix_origins::with_child(node, "type", |node| {
            super::routine_origins::type_origin(node, ty)
        });
    }
}
