//! Origins of a regex's callable and aliased assertions (`<&wc>`,
//! `<wc=&wc>`, `<x=.alpha>`, `<x=word("a")>`; ADR-10723 Stage 2, #15780), for the walk of a
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
                // `<alias=word("a")>`: a named assertion with arguments.
                RakuAstClass::RegexAssertionNamedArgs => {
                    let word = match super::name_parts::name_shape(child(assertion, "name")?)? {
                        super::name_parts::NameShape::Identifier(word) => word,
                        _ => return None,
                    };
                    // `<alias=.word("a")>`: a non-capturing call, its dot
                    // before the assertion.
                    let dot = usize::from(!capturing(assertion) && rest.starts_with('.'));
                    let (rest, start) = (&rest[dot..], start + dot);
                    let after = rest.strip_prefix(word.as_str())?;
                    // `word(ARGS)`, or `word: ARGS` to the assertion's `>`.
                    let (args, args_len, len) = match after.strip_prefix('(') {
                        Some(args) => {
                            let args_len = super::regex_call_origins::args_len(args)?;
                            (args, args_len, word.len() + 1 + args_len + 1)
                        }
                        None => {
                            let args = after.strip_prefix(':')?;
                            let args_len = super::regex_call_origins::colon_args_len(args)?;
                            (args, args_len, word.len() + 1 + args_len)
                        }
                    };
                    let args_at = start + word.len() + 1;
                    let mut placed = None;
                    with_child(node, "assertion", |named| {
                        placed = super::regex_call_origins::place(
                            &crate::scope::NameScope::default(),
                            named,
                            &args[..args_len],
                            args_at,
                        );
                        if placed.is_some() {
                            super::origin::set_span(named, SrcSpan::new(start, start + len));
                            with_child(named, "name", |n| {
                                super::origin::set_span(n, SrcSpan::new(start, start + word.len()))
                            });
                        }
                    });
                    placed?;
                    dot + len
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

/// The length of the callee of `callable` written at the start of `text`
/// (byte `at` of the unit): a variable (`&wc`) or an item contextualizer over
/// one (`&$p`), then its arguments in parentheses when it has any
/// (`&q("b")`), each placed there.
// Cost: O(v + a), v = length of the callee, a = length of the arguments,
// plus their parse.
fn callee(callable: &mut RakuAstNode, text: &str, at: usize) -> Option<usize> {
    let held = child(callable, "callee")?;
    // `<&$p>`: an item contextualizer over the variable, written `&` then
    // the variable.
    let (written, inner) = match held.class {
        RakuAstClass::ContextualizerItem => {
            let var = leaf_text(super::infix_origins::child_at(held, 0)?)?;
            (format!("&{var}"), Some(var.len()))
        }
        _ => (leaf_text(held)?, None),
    };
    let rest = text.strip_prefix(written.as_str())?;
    let has_args = callable.fields.iter().any(|f| f.name == Some("args"));
    let len = if has_args {
        // `<&q("b")>`: the arguments in parentheses right after the callee.
        let args = rest.strip_prefix('(')?;
        let args_len = super::regex_call_origins::args_len(args)?;
        super::regex_call_origins::place(
            &crate::scope::NameScope::default(),
            callable,
            &args[..args_len],
            at + written.len() + 1,
        )?;
        written.len() + 1 + args_len + 1
    } else {
        if rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '(')) {
            return None;
        }
        written.len()
    };
    with_child(callable, "callee", |v| {
        super::origin::set_span(v, SrcSpan::new(at, at + written.len()));
        if let Some(var) = inner {
            let end = at + written.len();
            super::infix_origins::with_child_at(v, 0, |var_node| {
                super::origin::set_span(var_node, SrcSpan::new(end - var, end))
            });
        }
    });
    Some(len)
}
