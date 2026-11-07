//! Production hooks (ADR-15762 §2.1): a slang's new `statement-control:sym<...>`
//! token is tried where the Rust parser starts a statement, ahead of the
//! built-in statement parsers, by running the token and its action on the slang
//! host. The node the action attached is lowered here and stands for the
//! statement.
//!
//! The registry is unit state like [`super::slang_modes`]: reset at parse
//! start, snapshotted around a nested module scan, and filled only on the real
//! parse (the activation interpreter has no host to ask).

use super::*;
use crate::runtime::slang_host::SlangHost;
use std::cell::RefCell;
use std::rc::Rc;

/// One hookable production a slang added.
#[derive(Clone, Debug)]
pub(crate) struct HookRule {
    /// The token's name as the slang wrote it: `statement-control:sym<twice>`.
    pub(crate) rule: String,
    /// The literal the token must start with, read from its source, so that the
    /// host is asked only where the production can match.
    prefix: Option<String>,
}

#[derive(Clone, Default)]
pub(in crate::parser) struct SlangHooks {
    rules: Vec<HookRule>,
    host: Option<Rc<SlangHost>>,
}

thread_local! {
    static HOOKS: RefCell<SlangHooks> = RefCell::new(SlangHooks::default());
}

/// The category and symbol of a hookable token name (`statement-control:sym<x>`,
/// also spelled with an underscore), or `None` for any other rule.
// Cost: O(n), n = name length.
pub(crate) fn slang_hook_rule(name: &str) -> Option<&'static str> {
    let (category, rest) = name.split_once(":sym<")?;
    rest.strip_suffix('>')?;
    match category {
        "statement-control" | "statement_control" => Some("statement-control"),
        _ => None,
    }
}

/// The literal a token body starts with: `'twice' ...` or `"twice" ...`.
// Cost: O(n), n = body length.
fn leading_literal(body: &str) -> Option<String> {
    let body = body.trim_start();
    let quote = body.chars().next().filter(|c| matches!(c, '\'' | '"'))?;
    let inner = &body[1..];
    let end = inner.find(quote)?;
    let literal = &inner[..end];
    (!literal.is_empty() && !literal.contains('\\')).then(|| literal.to_string())
}

/// Register the hook for the override `name` with token source `body`.
// Cost: O(n), n = body length.
pub(in crate::parser) fn register_hook(name: &str, body: Option<&str>) {
    HOOKS.with(|h| {
        let mut hooks = h.borrow_mut();
        hooks.rules.retain(|r| r.rule != name);
        hooks.rules.push(HookRule {
            rule: name.to_string(),
            prefix: body.and_then(leading_literal),
        });
    });
}

/// The host that serves the registered hooks of this unit.
pub(in crate::parser) fn set_host(host: SlangHost) {
    HOOKS.with(|h| h.borrow_mut().host = Some(Rc::new(host)));
}

pub(in crate::parser) fn reset_hooks() {
    HOOKS.with(|h| *h.borrow_mut() = SlangHooks::default());
}

pub(in crate::parser) fn hooks_snapshot() -> SlangHooks {
    HOOKS.with(|h| h.borrow().clone())
}

pub(in crate::parser) fn restore_hooks(saved: SlangHooks) {
    HOOKS.with(|h| *h.borrow_mut() = saved);
}

/// The byte offset of character offset `chars` in `text`.
// Cost: O(n), n = the offset.
fn byte_offset(text: &str, chars: usize) -> usize {
    text.char_indices()
        .nth(chars)
        .map_or(text.len(), |(at, _)| at)
}

/// Try the slang's `statement-control` productions at `input`, the start of a
/// statement. `None`: no hook applies here (no slang, or no candidate matched).
// Cost: O(1) with no hook registered; otherwise the slang's own run.
pub(in crate::parser) fn statement_control_hook(input: &str) -> Option<PResult<'_, Stmt>> {
    let (rules, host) = HOOKS.with(|h| {
        let hooks = h.borrow();
        if hooks.rules.is_empty() {
            return (Vec::new(), None);
        }
        let rules: Vec<HookRule> = hooks
            .rules
            .iter()
            .filter(|r| {
                slang_hook_rule(&r.rule) == Some("statement-control")
                    && r.prefix.as_deref().is_none_or(|p| input.starts_with(p))
            })
            .cloned()
            .collect();
        (rules, hooks.host.clone())
    });
    let host = host?;
    for candidate in rules {
        let answer = host.run_hook(&candidate.rule, input, &mut |rule, pos| {
            crate::rakuast::slang_bridge::parse_production(rule, &input[byte_offset(input, pos)..])
                .map(|found| {
                    found.map(|(len, node)| {
                        let start = byte_offset(input, pos);
                        (input[..start + len].chars().count(), node)
                    })
                })
        });
        match answer {
            Ok(None) => continue,
            Ok(Some((end, node))) => {
                let rest = &input[byte_offset(input, end)..];
                return Some(
                    crate::rakuast::slang_bridge::lower_statement(&node)
                        .map(|stmt| (rest, stmt))
                        .map_err(|e| PError::fatal_at(e.message.to_string(), input)),
                );
            }
            Err(message) => {
                return Some(Err(PError::fatal_at(
                    format!("slang production '{}': {message}", candidate.rule),
                    input,
                )));
            }
        }
    }
    None
}

/// Parse the core production `rule` at the start of `text` for a slang token
/// that calls it (`<block>`, ADR-15762 §2.3): the text after it and the
/// statement it stands for. `None` for a production with no shim yet.
// Cost: the parse of the production.
pub(crate) fn parse_slang_shim_production<'a>(
    rule: &str,
    text: &'a str,
) -> Option<Result<(&'a str, Stmt), String>> {
    match rule {
        "block" => Some(super::block_stmt(text).map_err(|e| e.to_string())),
        _ => None,
    }
}
