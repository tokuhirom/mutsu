//! Origins of the left operand of an assigning infix (ADR-10723 Stage 2,
//! #15780): a declaration it assigns to as it declares it (`my $i **= $i`),
//! which the parser desugars, so it has no range: the scope and the variable
//! written at the start of the application; and a method call it assigns
//! to (`$pair.value += 100`), placed as an application of its own.

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

/// `$pair.value += 100`: a placed `ApplyPostfix` left operand whose call has
/// no range yet gets the call's ranges from its own.
// Cost: O(n), n = length of the operand's text.
pub(super) fn left_call(node: &mut RakuAstNode) {
    let Some(left) = child(node, "left") else {
        return;
    };
    if left.class != RakuAstClass::ApplyPostfix
        || child(left, "postfix").is_none_or(|p| p.origin.0.is_some())
    {
        return;
    }
    let Some((from, to)) = byte_range(left) else {
        return;
    };
    with_child(node, "left", |left| {
        super::call_origins::postfix_call(left, SrcSpan::new(from, to))
    });
}

/// `temp $_ = 42`: a word prefix over a variable, as the left operand at the
/// start of the application: the application, its `Prefix` and the variable.
// Cost: O(n), n = length of the operand's text.
pub(super) fn leading_prefix(node: &mut RakuAstNode) {
    use super::infix_origins::leaf_text;
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(left) = child(node, "left") else {
        return;
    };
    if left.class != RakuAstClass::ApplyPrefix || left.origin.0.is_some() {
        return;
    }
    let Some(op) = child(left, "prefix")
        .filter(|p| p.class == RakuAstClass::Prefix)
        .and_then(leaf_text)
        .filter(|op| op.chars().all(char::is_alphabetic))
    else {
        return;
    };
    let Some(operand) = child(left, "operand") else {
        return;
    };
    let Some(var) = (operand.class == RakuAstClass::VarLexical)
        .then(|| leaf_text(operand))
        .flatten()
    else {
        return;
    };
    let Some(var_at) = super::unit_source::with_text_from(from, |text| {
        let rest = text.strip_prefix(op.as_str())?;
        let blanks = rest.len() - rest.trim_start_matches([' ', '\t']).len();
        let after = rest[blanks..].strip_prefix(var.as_str())?;
        (blanks > 0 && !after.starts_with(|c: char| c.is_alphanumeric() || c == '_'))
            .then(|| from + op.len() + blanks)
    }) else {
        return;
    };
    with_child(node, "left", |left| {
        super::origin::set_span(left, SrcSpan::new(from, var_at + var.len()));
        with_child(left, "prefix", |p| {
            super::origin::set_span(p, SrcSpan::new(from, from + op.len()))
        });
        with_child(left, "operand", |v| {
            super::origin::set_span(v, SrcSpan::new(var_at, var_at + var.len()))
        });
    });
}
