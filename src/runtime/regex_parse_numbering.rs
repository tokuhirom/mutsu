//! Static positional-capture numbering for numbered aliases (`$N=`, #10895).
//!
//! Rakudo numbers a regex's positional captures statically, over the whole
//! capture level (NQP's `capnames`): a `( … )` takes the running counter, a
//! `$N=` sets the counter to `N` and takes slot `N`, each alternation branch
//! starts from the same counter and the alternation continues from the widest
//! branch, and a slot filled more than once (two aliases to one index, or a
//! capture under a quantifier) is a list. The engines number positional
//! captures dynamically, by push order, which agrees with that scheme only
//! while no `$N=` moves the counter.
//!
//! So a capture level that contains a numbered alias is renumbered here, once,
//! at parse time: every positional capture of that level is given its static
//! slot as an all-digit capture *name*. The engines file such a capture on the
//! named axis like any other alias — which already gives the right list-ness
//! for a name filled twice or under a quantifier, at every nesting depth and in
//! both engines — and a finished level moves its digit-named entries into the
//! positional axis (`regex_caps::settle_numbered_captures`). A level with no
//! numbered alias is left alone and keeps the dynamic numbering.

use crate::runtime::regex_types::{RegexAtom, RegexPattern, RegexToken};
use std::cell::Cell;

thread_local! {
    /// How many structural parses enclose the current one. Only an outermost
    /// parse holds a whole capture level, so only it is renumbered; the
    /// sub-pattern parses it recurses through (groups, alternation branches,
    /// separators) are numbered as part of it.
    static PARSE_DEPTH: Cell<u32> = const { Cell::new(0) };
}

/// Marks one structural parse in progress (see [`PARSE_DEPTH`]).
pub(crate) struct NumberingParseScope {
    outermost: bool,
}

impl NumberingParseScope {
    pub(crate) fn enter() -> Self {
        let depth = PARSE_DEPTH.with(|d| d.replace(d.get() + 1));
        NumberingParseScope {
            outermost: depth == 0,
        }
    }

    /// Whether this parse is not nested in another one.
    pub(crate) fn outermost(&self) -> bool {
        self.outermost
    }
}

impl Drop for NumberingParseScope {
    fn drop(&mut self) {
        PARSE_DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
    }
}

/// Marks an independent top-level parse (a regex value spliced in by
/// `<$var>` is parsed, and cached, as a regex of its own even while the
/// pattern around it is being parsed): it is outermost whatever encloses it.
pub(crate) struct IndependentParseScope {
    saved: u32,
}

impl IndependentParseScope {
    pub(crate) fn enter() -> Self {
        IndependentParseScope {
            saved: PARSE_DEPTH.with(|d| d.replace(0)),
        }
    }
}

impl Drop for IndependentParseScope {
    fn drop(&mut self) {
        PARSE_DEPTH.with(|d| d.set(self.saved));
    }
}

/// The slot an all-digit alias names.
fn numbered_alias(name: &str) -> Option<usize> {
    if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    name.parse().ok()
}

fn token_numbered_alias(token: &RegexToken) -> Option<usize> {
    token.named_capture.as_deref().and_then(numbered_alias)
}

/// Renumber every capture level of `pattern` that holds a numbered alias.
// Cost: O(t), t = the tokens in the pattern tree.
pub(crate) fn number_positional_captures(pattern: &mut RegexPattern) {
    number_level(&mut pattern.tokens);
}

/// One capture level: renumber it if it holds a numbered alias, then every
/// level nested in it (a capture group's body starts a level of its own).
fn number_level(tokens: &mut [RegexToken]) {
    if level_has_numbered_alias(tokens) {
        number_tokens(tokens, 0);
    }
    for_each_nested_level(tokens);
}

/// The same-level sub-patterns of `atom`: what a non-capturing construct
/// matches as part of the level it sits in.
fn same_level_parts(atom: &RegexAtom) -> Vec<&RegexPattern> {
    match atom {
        RegexAtom::Group(p) => vec![p],
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts.iter().collect(),
        RegexAtom::GoalMatch { goal, inner, .. } => vec![inner, goal],
        _ => Vec::new(),
    }
}

fn level_has_numbered_alias(tokens: &[RegexToken]) -> bool {
    tokens.iter().any(|token| {
        token_numbered_alias(token).is_some()
            || same_level_parts(&token.atom)
                .into_iter()
                .any(|p| level_has_numbered_alias(&p.tokens))
            || token
                .separator
                .as_ref()
                .is_some_and(|sep| level_has_numbered_alias(&sep.pattern.tokens))
    })
}

/// Visit the capture levels nested in this one: capture-group bodies, the
/// bodies of capture-isolated sub-matches and lookarounds, found through the
/// level's own non-capturing structure.
fn for_each_nested_level(tokens: &mut [RegexToken]) {
    for token in tokens {
        match &mut token.atom {
            RegexAtom::CaptureGroup(p)
            | RegexAtom::CaptureIsolatedGroup(p)
            | RegexAtom::CaptureIsolatedGroupScoped(p, _)
            | RegexAtom::Lookaround { pattern: p, .. } => number_level(&mut p.tokens),
            RegexAtom::Group(p) => for_each_nested_level(&mut p.tokens),
            RegexAtom::Alternation(alts)
            | RegexAtom::SequentialAlternation(alts)
            | RegexAtom::Conjunction(alts) => {
                for alt in alts {
                    for_each_nested_level(&mut alt.tokens);
                }
            }
            RegexAtom::GoalMatch { goal, inner, .. } => {
                for_each_nested_level(&mut inner.tokens);
                for_each_nested_level(&mut goal.tokens);
            }
            _ => {}
        }
        if let Some(sep) = &mut token.separator {
            for_each_nested_level(&mut sep.pattern.tokens);
        }
    }
}

/// Number `tokens` from `count` (NQP `capnames`), returning the counter after
/// them.
fn number_tokens(tokens: &mut [RegexToken], mut count: usize) -> usize {
    for token in tokens {
        count = number_token(token, count);
    }
    count
}

fn number_token(token: &mut RegexToken, mut count: usize) -> usize {
    let explicit = token_numbered_alias(token);
    if let Some(n) = explicit {
        count = n + 1;
    }
    match &mut token.atom {
        RegexAtom::CaptureGroup(_) => {
            // `$<name>=( … )` is named only; `$N=( … )` already set the counter.
            if token.named_capture.is_none() {
                token.named_capture = Some(count.to_string());
                count += 1;
            }
        }
        RegexAtom::Group(p) => count = number_tokens(&mut p.tokens, count),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            let start = count;
            for alt in alts {
                count = count.max(number_tokens(&mut alt.tokens, start));
            }
        }
        RegexAtom::Conjunction(parts) => {
            for part in parts {
                count = number_tokens(&mut part.tokens, count);
            }
        }
        RegexAtom::GoalMatch { goal, inner, .. } => {
            count = number_tokens(&mut inner.tokens, count);
            count = number_tokens(&mut goal.tokens, count);
        }
        // A backreference reads the slot by its static number, which is now
        // filed under that number's name.
        RegexAtom::Backref(n) => token.atom = RegexAtom::NamedBackref(n.to_string()),
        _ => {}
    }
    if let Some(sep) = &mut token.separator {
        count = number_tokens(&mut sep.pattern.tokens, count);
    }
    count
}
