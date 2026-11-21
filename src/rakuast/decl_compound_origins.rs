//! Origins of a declaration an operator assigns to as it declares it
//! (ADR-10723 Stage 2, #15780): `my $i **= $i`. The parser desugars the
//! declaration, so the left operand has no range; it is the scope and the
//! variable written at the start of the application.

use super::infix_origins::{byte_range, child, with_child};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::ValueView;

/// Place the unplaced `VarDeclaration::Simple` left operand of `node`, an
/// `ApplyInfix` written from the declaration's scope, when it has no type or
/// traits: `my $i` at the start of the application.
// Cost: O(n), n = length of the declaration's text.
pub(super) fn leading_declaration(node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(left) = child(node, "left") else {
        return;
    };
    if left.class != RakuAstClass::VarDeclarationSimple || left.origin.0.is_some() {
        return;
    }
    let leaf = |name: &str| {
        left.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        })
    };
    if left
        .fields
        .iter()
        .any(|f| matches!(f.name, Some("type" | "traits" | "initializer" | "twigil")))
    {
        return;
    }
    let scope = leaf("scope").unwrap_or_else(|| "my".to_string());
    let (Some(sigil), Some(name)) = (
        leaf("sigil"),
        super::infix_origins::child(left, "desigilname").and_then(|n| {
            match super::name_parts::name_shape(n)? {
                super::name_parts::NameShape::Identifier(name) => Some(name),
                _ => None,
            }
        }),
    ) else {
        return;
    };
    let variable = format!("{sigil}{name}");
    let Some(len) = super::unit_source::with_text_from(from, |text| {
        let rest = text.strip_prefix(scope.as_str())?;
        let blanks = rest.len() - rest.trim_start_matches([' ', '\t']).len();
        let after = rest[blanks..].strip_prefix(variable.as_str())?;
        (blanks > 0
            && !after.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'')))
        .then(|| scope.len() + blanks + variable.len())
    }) else {
        return;
    };
    with_child(node, "left", |left| {
        super::origin::set_span(left, SrcSpan::new(from, from + len))
    });
}
