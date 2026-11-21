//! Origins of a regex's callable and aliased assertions (`<&wc>`,
//! `<wc=&wc>`, `<x=.alpha>`; ADR-10723 Stage 2, #15780), for the walk of a
//! regex body (`regex_origins`).

use super::infix_origins::{child, leaf_text, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// The length of `node`, a `Regex::Assertion::Callable` or
/// `Regex::Assertion::Alias` written at the start of `text` (byte `at` of
/// the unit), after giving the nodes inside it their ranges. `None` when
/// `text` does not spell it; the caller places `node` itself.
// Cost: O(n), n = length of the assertion's text.
pub(super) fn place(node: &mut RakuAstNode, text: &str, at: usize) -> Option<usize> {
    let inner = text.strip_prefix('<')?;
    let len = match node.class {
        // `<&wc>`: the callee inside the brackets.
        RakuAstClass::RegexAssertionCallable => callee(node, inner, at + 1)?,
        // `<wc=&wc>` / `<x=.alpha>`: the name, `=`, then the assertion
        // without brackets of its own.
        RakuAstClass::RegexAssertionAlias => {
            let name = leaf_text(node)?;
            let rest = inner.strip_prefix(name.as_str())?.strip_prefix('=')?;
            let start = at + 1 + name.len() + 1;
            let assertion = child(node, "assertion")?;
            let len = match assertion.class {
                RakuAstClass::RegexAssertionCallable => {
                    let mut len = None;
                    with_child(node, "assertion", |callable| {
                        len = callee(callable, rest, start);
                        if let Some(len) = len {
                            super::origin::set_span(callable, SrcSpan::new(start, start + len));
                        }
                    });
                    len?
                }
                RakuAstClass::RegexAssertionNamed if !capturing(assertion) => {
                    let word = match super::name_parts::name_shape(child(assertion, "name")?)? {
                        super::name_parts::NameShape::Identifier(word) => word,
                        _ => return None,
                    };
                    rest.strip_prefix('.')?.strip_prefix(word.as_str())?;
                    let span = SrcSpan::new(start + 1, start + 1 + word.len());
                    with_child(node, "assertion", |named| {
                        super::origin::set_span(named, span);
                        with_child(named, "name", |n| super::origin::set_span(n, span));
                    });
                    1 + word.len()
                }
                _ => return None,
            };
            name.len() + 1 + len
        }
        _ => return None,
    };
    inner[len..].starts_with('>').then_some(len + 2)
}

/// Whether a named assertion captures (`<alpha>`, not `<.alpha>`).
// Cost: O(f), f = fields of the node.
fn capturing(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|f| f.name == Some("capturing"))
}

/// The length of the callee variable (`&wc`) of `callable` written at the
/// start of `text` (byte `at` of the unit), placed there. A callable with
/// arguments is not read.
// Cost: O(v), v = length of the variable's name.
fn callee(callable: &mut RakuAstNode, text: &str, at: usize) -> Option<usize> {
    if callable.fields.iter().any(|f| f.name == Some("args")) {
        return None;
    }
    let var = leaf_text(child(callable, "callee")?)?;
    let rest = text.strip_prefix(var.as_str())?;
    if rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '(')) {
        return None;
    }
    with_child(callable, "callee", |v| {
        super::origin::set_span(v, SrcSpan::new(at, at + var.len()))
    });
    Some(var.len())
}
