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
