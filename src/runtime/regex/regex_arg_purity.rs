//! Purity tracking for the parameterized-subrule memo
//! ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
//!
//! Resolving `<rule($arg)>` binds the arguments into a scratch interpreter,
//! evaluates the rule body, and runs three textual passes over the resulting
//! pattern before parsing it. `PARSED_TOKEN_ARG_CANDIDATES` memoizes the whole
//! chain on `(package, rule name, rendered arguments)` plus the token-registry
//! generation, so it may only store a result that is a function of exactly
//! that key.
//!
//! The exclusions are raised **at the reads themselves** rather than derived
//! from a syntactic predicate over the pattern text. That is round 13's lesson
//! on the sibling `REGEX_SUBPATTERN_PARSE_CACHE` (see
//! [`crate::runtime::regex_parse::PARSE_CONSULTED_AMBIENT_STATE`]): a list of
//! "syntax forms that read state", maintained next to the code that reads it,
//! cannot stay correct as that code grows — the list this replaces
//! (`pattern_static_modulo_params`) conservatively rejected every `$<capture>`
//! form, which is most of a real grammar, while silently trusting the
//! evaluation of the rule body and of the argument expressions, which can read
//! anything at all.
//!
//! Two things raise the flag:
//!
//! * a *named* read whose name is not one of the bound parameters — the value
//!   comes from the caller's lexical scope, which the key does not carry;
//! * an *opaque* read — a step that runs arbitrary code (a rule body that is
//!   not a constant, an argument expression that needs the evaluator, a
//!   parameter default or `where` clause) and whose reads therefore cannot be
//!   attributed.
//!
//! A parse-time ambient read raises this flag too, via
//! [`crate::runtime::Interpreter::note_regex_parse_ambient_read`]: the memo
//! stores the *parsed* candidates, so an impure parse makes the entry impure.

use std::cell::{Cell, RefCell};

thread_local! {
    /// Set when the resolution in progress read state its memo key does not
    /// carry. Saved/cleared/OR-restored around each resolution exactly the way
    /// `parse_regex_uncached` handles its own flag, so a nested resolution's
    /// impurity propagates outward to the enclosing one.
    static CONSULTED_AMBIENT_STATE: Cell<bool> = const { Cell::new(false) };

    /// The bound parameter names of the resolutions currently in progress,
    /// innermost last. Empty when no parameterized subrule is being resolved,
    /// which is what makes every note below a no-op for the ordinary
    /// (uncached) callers of the same interpolation helpers.
    static BOUND_PARAMS: RefCell<Vec<Vec<String>>> = const { RefCell::new(Vec::new()) };
}

/// Raise the flag unconditionally: a step that can read state we cannot name.
pub(crate) fn note_opaque_read() {
    CONSULTED_AMBIENT_STATE.with(|f| f.set(true));
}

/// Raise the flag unless `name` is a bound parameter of every resolution in
/// progress.
///
/// `name` arrives as the pattern spelled it, with or without its sigil, and the
/// parameter list holds the declared spellings (`$indent`, `:$sep`), so both
/// sides are compared with sigils and adverb punctuation trimmed.
pub(crate) fn note_named_read(name: &str) {
    let ambient = BOUND_PARAMS.with(|p| {
        let frames = p.borrow();
        if frames.is_empty() {
            return false;
        }
        let bare = trim_sigils(name);
        frames
            .iter()
            .any(|params| !params.iter().any(|p| trim_sigils(p) == bare))
    });
    if ambient {
        note_opaque_read();
    }
}

fn trim_sigils(name: &str) -> &str {
    name.trim_start_matches([':', '$', '@', '%', '&', '!', '.', '*', '?', '^'])
}

/// Run `body` as one memoizable resolution of a rule with `params`.
///
/// Returns `(value, consulted_ambient_state)`. The enclosing resolution's flag
/// is restored with this one OR-ed in, so an inner impurity is never lost.
pub(crate) fn with_resolution<T>(params: Vec<String>, body: impl FnOnce() -> T) -> (T, bool) {
    BOUND_PARAMS.with(|p| p.borrow_mut().push(params));
    let outer = CONSULTED_AMBIENT_STATE.with(|f| f.replace(false));
    let value = body();
    let consulted = CONSULTED_AMBIENT_STATE.with(|f| f.replace(outer || f.get()));
    BOUND_PARAMS.with(|p| {
        p.borrow_mut().pop();
    });
    (value, consulted)
}

/// Run `body` as the window a memo entry would cover, without declaring any
/// bound parameters of its own.
///
/// This is the outer half of [`with_resolution`]: the resolution frames report
/// the *rule bodies*' reads, and this window additionally covers the steps that
/// run outside them — notably parsing each resolved candidate, whose own
/// ambient reads reach here through
/// [`crate::runtime::Interpreter::note_regex_parse_ambient_read`].
pub(crate) fn with_memo_window<T>(body: impl FnOnce() -> T) -> (T, bool) {
    let outer = CONSULTED_AMBIENT_STATE.with(|f| f.replace(false));
    let value = body();
    let consulted = CONSULTED_AMBIENT_STATE.with(|f| f.replace(outer || f.get()));
    (value, consulted)
}
