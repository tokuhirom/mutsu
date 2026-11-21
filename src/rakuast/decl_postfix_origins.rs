//! Origin of a plain declaration used as a postfix operand
//! (`my $k.defined`): `my $k` is the operand, and the call follows it. Only
//! the exact spelling `SCOPE blanks VARIABLE` at the start of the
//! application is placed: exact or absent.

use super::infix_origins::with_child;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the operand of `node`, an `ApplyPostfix` written at `span`, when it
/// is a bare variable declaration without a range of its own.
// Cost: O(n), n = length of the application's source.
pub(super) fn operand(node: &mut RakuAstNode, span: SrcSpan) {
    chained(node, span);
    with_child(node, "operand", |decl| {
        if decl.class != RakuAstClass::VarDeclarationSimple || decl.origin.0.is_some() {
            return;
        }
        let (Some(sigil), Some(name)) = (
            super::decl_origins::leaf(decl, "sigil"),
            super::infix_origins::child(decl, "desigilname")
                .and_then(super::infix_origins::leaf_text),
        ) else {
            return;
        };
        let twigil = super::decl_origins::leaf(decl, "twigil").unwrap_or_default();
        let variable = format!("{sigil}{twigil}{name}");
        let len = super::unit_source::with_text(span, |text| {
            let word_len = text.find(char::is_whitespace)?;
            if !matches!(&text[..word_len], "my" | "our" | "state" | "anon") {
                return None;
            }
            let rest = text[word_len..].trim_start();
            let after = rest.strip_prefix(variable.as_str())?;
            // The name must end there: a postfix dot, a blank or the end.
            (!after.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-')))
                .then(|| text.len() - after.len())
        });
        if let Some(len) = len {
            super::origin::set_span(
                decl,
                SrcSpan::new(span.from as usize, span.from as usize + len),
            );
        }
    });
}

/// `my $n.WHAT.say`: an unplaced postfix application over a declaration,
/// under `node`, an `ApplyPostfix` written at `span` whose own postfix is an
/// argument-less method call (`.say`, `.^name`) ending the text. The inner
/// application runs from the start to that call's dot, and is placed as an
/// application written there.
// Cost: O(n * d), n = length of the application's source, d = depth of the
// chain.
fn chained(node: &mut RakuAstNode, span: SrcSpan) {
    let inner_over_decl = super::infix_origins::child(node, "operand").is_some_and(|inner| {
        inner.class == RakuAstClass::ApplyPostfix && inner.origin.0.is_none() && over_decl(inner)
    });
    let Some(postfix) = super::infix_origins::child(node, "postfix") else {
        return;
    };
    let dot = match postfix.class {
        RakuAstClass::CallMethod => ".",
        RakuAstClass::CallMetaMethod => ".^",
        _ => return,
    };
    let has_args =
        super::infix_origins::child(postfix, "args").is_some_and(|a| !a.fields.is_empty());
    if !inner_over_decl || has_args {
        return;
    }
    // A method call's `Name`, or a meta-method's plain string name.
    let Some(name) = super::infix_origins::child(postfix, "name")
        .and_then(super::infix_origins::leaf_text)
        .or_else(|| super::decl_origins::leaf(postfix, "name"))
    else {
        return;
    };
    let Some(cut) = super::unit_source::with_text(span, |text| {
        let head = text.strip_suffix(name.as_str())?.strip_suffix(dot)?;
        (!head.is_empty()).then_some(head.len())
    }) else {
        return;
    };
    let inner_span = SrcSpan::new(span.from as usize, span.from as usize + cut);
    with_child(node, "operand", |inner| {
        super::origin::set_span(inner, inner_span);
        super::call_origins::postfix_call(inner, inner_span);
    });
}

/// Whether the innermost operand of the postfix chain `node` is an unplaced
/// variable declaration.
// Cost: O(d), d = depth of the chain.
fn over_decl(node: &RakuAstNode) -> bool {
    match super::infix_origins::child(node, "operand") {
        Some(o) if o.class == RakuAstClass::VarDeclarationSimple => o.origin.0.is_none(),
        Some(o) if o.class == RakuAstClass::ApplyPostfix && o.origin.0.is_none() => over_decl(o),
        _ => false,
    }
}
