use super::super::*;
use rustc_hash::FxHashMap as HashMap;
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;
use unicode_normalization::char::is_combining_mark;
use unicode_segmentation::UnicodeSegmentation;

thread_local! {
    pub(super) static PENDING_REGEX_GOAL_FAILURE: RefCell<Option<(String, usize)>> = const { RefCell::new(None) };
    /// Declarative-prefix (LTM) mode. While set, the matcher is measuring *how far
    /// a candidate declaratively matches*, not producing a match — so a code atom
    /// (`{ … }`, `<?{ … }>`, `<!{ … }>`) must NOT be executed. Rakudo builds its LTM
    /// NFA from the declarative prefix and a code atom terminates that prefix, so
    /// on reaching one the matcher sets `LTM_PREFIX_TERMINATED` and unwinds,
    /// reporting the position it reached. Running the code instead would duplicate
    /// its side effects once per candidate measurement (ADR-0009).
    ///
    /// Set only around `declarative_prefix_match_len`, which restores the previous
    /// value afterwards (a subrule's pattern may itself be measured while an outer
    /// measurement is live).
    pub(crate) static LTM_DECLARATIVE_MODE: Cell<bool> = const { Cell::new(false) };
    /// A quantified alternation folds only captures it actually produces.
    /// Its unmatched branches must not manufacture extra iterations.
    pub(super) static IN_QUANTIFIED_ALTERNATION_MATCH: Cell<bool> = const { Cell::new(false) };
    /// Set by the matcher when `LTM_DECLARATIVE_MODE` made it stop at a code atom.
    /// `walk_tokens` checks it after every token and stops walking, so the
    /// termination propagates out through nested subrules.
    pub(crate) static LTM_PREFIX_TERMINATED: Cell<bool> = const { Cell::new(false) };
    /// Set by the `SequentialAlternation` (`X || Y`) measurement when its
    /// zero-width epsilon bypass was offered (ADR-0022 §4.2). It marks the
    /// measurement's `None` result as UNSOUND to filter on, without truncating
    /// the measured prefix the way `LTM_PREFIX_TERMINATED` does.
    ///
    /// Why the distinction matters: the epsilon lets the walk continue past a
    /// `||` group whose first branch did not match, but it continues at the
    /// group's *start* position, so every atom after the group is then measured
    /// against text the real match would have consumed. Such an atom can fail
    /// and drive the whole measurement to `None` even though the candidate
    /// matches perfectly well — Cro::Uri's `regex host:sym<IPv6address>
    /// { '[' <( <.IPv6address> )> ']' }`, whose `IPv6address` is one big `||`
    /// chain, measured `None` and was wrongly dropped as "cannot match here".
    /// Ranking on the (short) measured length is still fine; only the
    /// `(None, false)` FILTER is unsound, which is exactly what this flag
    /// suppresses (ADR-0046 Slice 4).
    pub(crate) static LTM_SEQALT_EPSILON: Cell<bool> = const { Cell::new(false) };
    /// Code atoms are inert: not executed, and treated as a zero-width pass.
    ///
    /// Set around `longest_complete_prefix_end`, which re-matches the pattern
    /// against every prefix of the input (longest first) purely to report *how far*
    /// a failed `.parse` got. That is a diagnostic, and computing a diagnostic must
    /// not run the user's code — with `<?{ … }>` in the grammar the probe executed
    /// it once per prefix, i.e. O(input length) times (ADR-0009). Unlike
    /// `LTM_DECLARATIVE_MODE` a plain `{ … }` block does not stop the walk here: the
    /// probe wants the longest prefix the pattern's declarative skeleton accepts, so
    /// both kinds of code atom simply become no-ops.
    pub(crate) static CODE_ATOMS_INERT: Cell<bool> = const { Cell::new(false) };
    /// Names declared by the currently executing grammar-rule dynamic-variable
    /// frames.  A `RegexAtom::VarDecl` normally evaluates its initializer when
    /// the cursor reaches it; grammar rule declarations are initialized by the
    /// rule-entry frame instead, so that the initializer runs once per
    /// invocation and is restored when that invocation leaves.  This stack is
    /// deliberately thread-local: the matcher can re-enter a nested parse on
    /// the same OS thread, and a frame must not make an unrelated interpreter's
    /// regex declaration look already initialized.
    pub(crate) static GRAMMAR_DYNVAR_SCOPE_KEYS: RefCell<Vec<std::collections::HashSet<String>>> =
        const { RefCell::new(Vec::new()) };
    /// Parse-scoped overlay of `$*` dynamic-variable values written by grammar
    /// action methods that run at *reduce time* (during matching). The regex
    /// match engine is `&self`, so an action's dyn-var write (e.g.
    /// Template::Mustache's delimiter finalizer `($*LEFT,$*RIGHT)=@delim`) cannot
    /// mutate `self.env` mid-match. Instead the reduce-time action runs in a
    /// scratch interpreter and its changed `$*` vars are published here;
    /// `interpolate_regex_scalars` consults this overlay BEFORE `self.env` so
    /// subsequent subrule matches re-interpolate their patterns with the new
    /// values. `Some` only while a grammar-actions parse is live (the common
    /// non-grammar case stays `None` for zero lookup cost). Keyed by the env name
    /// form (sigil-less, twigil-kept: `$*LEFT` -> `"*LEFT"`).
    pub(crate) static REGEX_DYNVAR_OVERLAY: RefCell<Option<HashMap<String, Value>>> = const { RefCell::new(None) };
    /// Set true once `interpolate_regex_scalars` resolves a `$*` dynamic var
    /// while a grammar-actions parse is live — i.e. the grammar's matching
    /// actually depends on a dynamic variable. The reduce-time action hook only
    /// fires when this is true, so ordinary (non-dyn-var) grammars pay nothing.
    pub(crate) static REGEX_GRAMMAR_DYNVAR_SEEN: Cell<bool> = const { Cell::new(false) };
    /// Log of every named subrule that *reduced* (matched successfully) during the
    /// live `Grammar.parse(:actions(...))`, in reduce order (children before their
    /// parent, since the matcher recurses). Rakudo dispatches an action method the
    /// moment its rule matches and never un-dispatches it when the surrounding
    /// pattern later backtracks; mutsu instead walks the finished match tree, so a
    /// parse that FAILS overall used to run no actions at all even though several
    /// subrules had matched. This log lets the failure path replay them. `Some`
    /// only while an action-driven parse is live.
    pub(crate) static REDUCED_SUBRULES: RefCell<Option<ReducedSubruleLog>> = const { RefCell::new(None) };
    /// In-regex `:my`/`:let` lexicals to seed the *next* capture store with.
    ///
    /// A sub-pattern that is lexically part of the same regex — a lookaround, a
    /// group, an alternative — is matched with a fresh `CapStore`, which would
    /// otherwise lose the `:my` lexicals declared before it (YAMLish computes its
    /// block indent in a `{ … }` inside a `<?before …>` and reads it back after).
    /// A *subrule* is a different regex and must NOT inherit them, so every other
    /// atom arms this *empty* for the duration of its match.
    pub(crate) static INLINE_REGEX_VARS_SEED: RefCell<Option<std::sync::Arc<crate::runtime::RegexVarMap>>> = const { RefCell::new(None) };
    /// Whether [`INLINE_REGEX_VARS_SEED`] currently holds anything. Read once per
    /// atom match to skip the `RefCell` entirely on the overwhelmingly common path
    /// where no regex in flight declares a `:my`/`:let` lexical.
    pub(crate) static INLINE_REGEX_VARS_ACTIVE: Cell<bool> = const { Cell::new(false) };
    /// The enclosing pattern level's captures to seed the *next* capture store
    /// with, so a backreference written inside an inline sub-pattern still sees
    /// them (see [`crate::runtime::OuterBackrefCaps`]). Armed *empty* by every
    /// non-inline atom — a subrule reference above all — which is what keeps a
    /// different regex's backreferences scoped to itself.
    pub(crate) static INLINE_OUTER_CAPS_SEED: RefCell<Option<std::sync::Arc<OuterBackrefCaps>>> = const { RefCell::new(None) };
}

/// Marker for one live grammar-rule dynamic-variable frame.
pub(crate) struct GrammarDynvarScopeGuard;

impl GrammarDynvarScopeGuard {
    pub(crate) fn enter(keys: impl IntoIterator<Item = String>) -> Self {
        GRAMMAR_DYNVAR_SCOPE_KEYS.with(|stack| {
            stack.borrow_mut().push(keys.into_iter().collect());
        });
        Self
    }
}

impl Drop for GrammarDynvarScopeGuard {
    fn drop(&mut self) {
        GRAMMAR_DYNVAR_SCOPE_KEYS.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

/// Whether a dynamic declaration is owned by a live grammar-rule frame.
pub(crate) fn grammar_dynvar_scope_active(name: &str) -> bool {
    GRAMMAR_DYNVAR_SCOPE_KEYS
        .with(|stack| stack.borrow().iter().rev().any(|keys| keys.contains(name)))
}

/// Set the first time the regex parser lowers a `$0` / `$<name>` backreference
/// atom anywhere in the process. Until then, none of the outer-capture seeding
/// below can matter, so the whole mechanism (including the per-atom sub-pattern
/// scan) is skipped with a single relaxed load. Process-global rather than
/// thread-local on purpose: a grammar is often parsed on one thread and matched
/// on another.
pub(crate) static REGEX_BACKREF_LOWERED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Called by the regex parser when it emits a backreference atom.
pub(crate) fn note_regex_backref_lowered() {
    REGEX_BACKREF_LOWERED.store(true, std::sync::atomic::Ordering::Relaxed);
}

pub(crate) fn any_regex_backref_lowered() -> bool {
    REGEX_BACKREF_LOWERED.load(std::sync::atomic::Ordering::Relaxed)
}

/// Arms [`INLINE_OUTER_CAPS_SEED`] for the duration of one atom match,
/// restoring the enclosing atom's seed on drop.
pub(crate) struct OuterCapsSeed {
    prev: Option<std::sync::Arc<OuterBackrefCaps>>,
    armed: bool,
}

impl OuterCapsSeed {
    /// Publish `next` (or, with `None`, a barrier that hides every enclosing
    /// level) for the nested stores this atom's match is about to build.
    pub(crate) fn arm(next: Option<std::sync::Arc<OuterBackrefCaps>>) -> Self {
        let prev = INLINE_OUTER_CAPS_SEED.with(|s| std::mem::replace(&mut *s.borrow_mut(), next));
        OuterCapsSeed { prev, armed: true }
    }

    /// Leave the enclosing atom's seed in place untouched.
    #[inline]
    pub(crate) fn inert() -> Self {
        OuterCapsSeed {
            prev: None,
            armed: false,
        }
    }
}

impl Drop for OuterCapsSeed {
    #[inline]
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        let prev = self.prev.take();
        INLINE_OUTER_CAPS_SEED.with(|s| *s.borrow_mut() = prev);
    }
}

/// The enclosing-level captures a freshly built capture store should read
/// backreferences through (see [`INLINE_OUTER_CAPS_SEED`]).
pub(crate) fn take_inline_outer_caps_seed() -> Option<std::sync::Arc<OuterBackrefCaps>> {
    if !any_regex_backref_lowered() {
        return None;
    }
    INLINE_OUTER_CAPS_SEED.with(|s| s.borrow().clone())
}

/// Does this atom's sub-pattern contain a backreference anywhere inside it?
/// Only such an atom needs to pay for snapshotting the enclosing captures.
pub(crate) fn atom_contains_backref(atom: &RegexAtom) -> bool {
    fn pattern_has(pattern: &RegexPattern) -> bool {
        *pattern.derived.contains_backref.get_or_init(|| {
            pattern.tokens.iter().any(|tok| {
                atom_contains_backref(&tok.atom)
                    || tok
                        .separator
                        .as_ref()
                        .is_some_and(|sep| pattern_has(&sep.pattern))
            })
        })
    }
    match atom {
        RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => true,
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::Lookaround { pattern: p, .. } => pattern_has(p),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts.iter().any(pattern_has),
        RegexAtom::GoalMatch { goal, inner, .. } => pattern_has(goal) || pattern_has(inner),
        _ => false,
    }
}

/// Is anything published in [`INLINE_REGEX_VARS_SEED`] right now? A `false`
/// means an atom that has no lexicals of its own to publish cannot change what
/// any nested store would see, so it need not arm the seed at all.
#[inline]
pub(crate) fn inline_regex_vars_active() -> bool {
    INLINE_REGEX_VARS_ACTIVE.with(Cell::get)
}

/// Arms the [`INLINE_REGEX_VARS_SEED`] for the duration of one atom match,
/// restoring the enclosing atom's seed on drop. An atom that is an inline
/// sub-pattern arms it with the lexicals in scope; every other atom — a subrule
/// reference above all — arms it *empty*, which is what stops a `:my` lexical
/// from leaking into a different regex.
pub(crate) struct InlineVarsSeed {
    prev: Option<std::sync::Arc<crate::runtime::RegexVarMap>>,
    armed: bool,
}

impl InlineVarsSeed {
    /// Publish `vars` (already a shared handle — `None` means "publish
    /// nothing", which is what a subrule reference arms) for the duration of
    /// one atom match.
    #[inline]
    pub(crate) fn arm(vars: Option<&std::sync::Arc<crate::runtime::RegexVarMap>>) -> Self {
        let active = INLINE_REGEX_VARS_ACTIVE.with(Cell::get);
        if vars.is_none() && !active {
            // Nothing to publish and nothing published: the atom cannot change
            // what any nested store would see, so leave the slot untouched.
            return InlineVarsSeed {
                prev: None,
                armed: false,
            };
        }
        let next = vars.cloned();
        INLINE_REGEX_VARS_ACTIVE.with(|f| f.set(next.is_some()));
        let prev = INLINE_REGEX_VARS_SEED.with(|s| std::mem::replace(&mut *s.borrow_mut(), next));
        InlineVarsSeed { prev, armed: true }
    }

    /// Leave the enclosing atom's seed in place untouched. Equivalent to
    /// [`Self::arm`]`(None)` when nothing is published, without asking the
    /// thread-local whether anything is.
    #[inline]
    pub(crate) fn inert() -> Self {
        InlineVarsSeed {
            prev: None,
            armed: false,
        }
    }
}

impl Drop for InlineVarsSeed {
    #[inline]
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        let prev = self.prev.take();
        INLINE_REGEX_VARS_ACTIVE.with(|f| f.set(prev.is_some()));
        INLINE_REGEX_VARS_SEED.with(|s| *s.borrow_mut() = prev);
    }
}

/// The in-regex lexicals a freshly built capture store should start from (see
/// [`INLINE_REGEX_VARS_SEED`]).
pub(crate) fn take_inline_regex_vars_seed() -> Option<std::sync::Arc<crate::runtime::RegexVarMap>> {
    if !INLINE_REGEX_VARS_ACTIVE.with(Cell::get) {
        return None;
    }
    INLINE_REGEX_VARS_SEED.with(|s| s.borrow().clone())
}

thread_local! {
    /// Scalar names an enclosing `:my`/`:let` declared in the regex parse(s)
    /// currently in flight. A sub-pattern — a group, an alternative, a
    /// lookaround body — is parsed by a *nested* `parse_regex_uncached` call
    /// that would otherwise not know about them, and would both pre-substitute
    /// the outer-scope `$v` and fail to lower the bare `$v` to a match-time
    /// `VarInterp` atom (YAMLish's `block-string` matches its measured indent
    /// inside a quantified group: `[ $indent $new-indent … ]+ % <.line-break>`).
    static ENCLOSING_REGEX_VARS: RefCell<std::collections::HashSet<String>> =
        RefCell::new(std::collections::HashSet::new());
}

/// Restores [`ENCLOSING_REGEX_VARS`] to its entry state, so a sub-pattern's own
/// declarations do not leak back out to the pattern that contains it.
pub(crate) struct EnclosingRegexVarsGuard(std::collections::HashSet<String>);

impl EnclosingRegexVarsGuard {
    /// Snapshot the enclosing declarations; the returned set is what a nested
    /// parse should start its own `declared_regex_vars` from.
    pub(crate) fn enter() -> (Self, std::collections::HashSet<String>) {
        let snapshot = ENCLOSING_REGEX_VARS.with(|s| s.borrow().clone());
        (Self(snapshot.clone()), snapshot)
    }
}

impl Drop for EnclosingRegexVarsGuard {
    fn drop(&mut self) {
        ENCLOSING_REGEX_VARS.with(|s| *s.borrow_mut() = std::mem::take(&mut self.0));
    }
}

/// Normalize a `<name>` / `<.name>` / `<&name>` subrule lookup name and check
/// whether it names the `ws` rule (implicit sigspace or an explicit `<.ws>` /
/// `<ws>` / method-form lookup). Shared between the regex parser's own
/// `<.ws>`-detection (`regex_parse_core.rs`'s `token_is_ws_like`, which needs
/// this to classify a token as ws-like at parse time) and the LTM
/// declarative-prefix mode's atom classifier (ADR-0022 §4.2: `ws` terminates
/// the prefix, same as an explicit code block).
pub(crate) fn named_lookup_is_ws(name: &str) -> bool {
    let mut raw = name.trim();
    if let Some(stripped) = raw.strip_prefix('.') {
        raw = stripped.trim();
    }
    if let Some(stripped) = raw.strip_prefix('&') {
        raw = stripped.trim();
    }
    raw == "ws"
}

/// Is this env key a dynamic variable (`$*x` is stored as `*x`; `@*a` / `%*h`
/// keep their sigil)? Such a name declared by an in-regex `:my` belongs to the
/// per-rule dynvar machinery, not to the regex-lexical snapshot/restore.
pub(crate) fn is_dynamic_regex_var_key(key: &str) -> bool {
    key.starts_with('*') || key.starts_with("@*") || key.starts_with("%*")
}

/// Publish a `:my`/`:let` scalar name to the sub-pattern parses that follow it.
pub(crate) fn declare_enclosing_regex_var(name: &str) {
    ENCLOSING_REGEX_VARS.with(|s| {
        s.borrow_mut().insert(name.to_string());
    });
}

/// Was `name` declared by a `:my`/`:let` in an enclosing pattern still being
/// parsed? Such a name must not be pre-substituted from the outer env.
pub(crate) fn is_enclosing_regex_var(name: &str) -> bool {
    ENCLOSING_REGEX_VARS.with(|s| s.borrow().contains(name))
}

/// Hard cap on `ReducedSubruleLog` entries. The log only feeds the *failure*
/// replay, so dropping the tail of a pathologically large parse costs nothing
/// but keeps a long action-driven parse from accumulating unbounded memory.
const REDUCED_SUBRULE_LOG_CAP: usize = 20_000;

/// Reduce-order log of named-subrule matches — see `REDUCED_SUBRULES`.
#[derive(Default)]
pub(crate) struct ReducedSubruleLog {
    /// `(rule name to dispatch the action under, that rule's captures)`.
    entries: Vec<(String, std::sync::Arc<CapNode>)>,
    /// De-dups `(rule, from, to)`: mutsu's matcher enumerates every candidate end
    /// position of a subrule, so the same reduce is often produced repeatedly.
    seen: std::collections::HashSet<(String, usize, usize)>,
}

impl ReducedSubruleLog {
    pub(crate) fn into_entries(self) -> Vec<(String, std::sync::Arc<CapNode>)> {
        self.entries
    }
}

/// Record a named subrule's successful match for the failure-path action replay.
/// No-op unless an action-driven parse is live, and never while the matcher is
/// only *measuring* a declarative prefix or probing the failure position
/// (ADR-0009: those passes must have no observable side effects).
pub(crate) fn record_reduced_subrule(rule: &str, caps: &std::sync::Arc<CapNode>) {
    if LTM_DECLARATIVE_MODE.with(Cell::get) || CODE_ATOMS_INERT.with(Cell::get) {
        return;
    }
    REDUCED_SUBRULES.with(|slot| {
        let mut slot = slot.borrow_mut();
        let Some(log) = slot.as_mut() else {
            return;
        };
        if log.entries.len() >= REDUCED_SUBRULE_LOG_CAP {
            return;
        }
        if log.seen.insert((rule.to_string(), caps.from, caps.to)) {
            log.entries.push((rule.to_string(), caps.clone()));
        }
    });
}

/// Activates the reduce log for one `Grammar.parse(:actions(...))`, restoring any
/// enclosing parse's log on drop.
pub(crate) struct ReducedSubruleGuard {
    prev: Option<ReducedSubruleLog>,
}

impl ReducedSubruleGuard {
    pub(crate) fn activate() -> Self {
        let prev =
            REDUCED_SUBRULES.with(|slot| slot.borrow_mut().replace(ReducedSubruleLog::default()));
        ReducedSubruleGuard { prev }
    }

    /// Take the entries logged so far, leaving a fresh empty log in place.
    pub(crate) fn take_entries() -> Vec<(String, std::sync::Arc<CapNode>)> {
        REDUCED_SUBRULES.with(|slot| {
            let mut slot = slot.borrow_mut();
            match slot.as_mut() {
                Some(log) => std::mem::take(log).into_entries(),
                None => Vec::new(),
            }
        })
    }
}

impl Drop for ReducedSubruleGuard {
    fn drop(&mut self) {
        REDUCED_SUBRULES.with(|slot| *slot.borrow_mut() = self.prev.take());
    }
}

/// Look up a `$*` dynamic var in the reduce-time overlay (see
/// `REGEX_DYNVAR_OVERLAY`). `name` is the env form without sigil (e.g. `*LEFT`).
/// Returns `None` when the overlay is inactive or has no entry for the name.
pub(crate) fn dynvar_overlay_get(name: &str) -> Option<Value> {
    REGEX_DYNVAR_OVERLAY.with(|slot| slot.borrow().as_ref().and_then(|m| m.get(name).cloned()))
}

/// True while a grammar-actions parse has an active dyn-var overlay.
pub(crate) fn dynvar_overlay_active() -> bool {
    REGEX_DYNVAR_OVERLAY.with(|slot| slot.borrow().is_some())
}

/// Record that the live grammar parse interpolated a `$*` dynamic var, enabling
/// the reduce-time action hook for the rest of the parse.
pub(crate) fn dynvar_mark_seen() {
    REGEX_GRAMMAR_DYNVAR_SEEN.with(|c| c.set(true));
}

/// Whether any `$*` dynamic var has been interpolated during the live parse.
pub(crate) fn dynvar_seen() -> bool {
    REGEX_GRAMMAR_DYNVAR_SEEN.with(|c| c.get())
}

/// Reset the overlay to empty (and clear the SEEN flag) at the start of a fresh
/// top-level grammar scan. A scan evolves the overlay left-to-right; a NEW scan
/// of the whole input (e.g. the candidate-selection pass vs the real match, or a
/// re-`parse`) must begin with the initial dynamic-var state from `self.env`,
/// not the values a previous scan left behind. No-op when the overlay is
/// inactive (non-grammar matching).
pub(crate) fn dynvar_overlay_reset_scan() {
    REGEX_DYNVAR_OVERLAY.with(|slot| {
        let mut b = slot.borrow_mut();
        if b.is_some() {
            *b = Some(HashMap::default());
        }
    });
    REGEX_GRAMMAR_DYNVAR_SEEN.with(|c| c.set(false));
}

/// Clone the current overlay contents (empty when inactive). Used to seed a
/// reduce-time action's scratch interpreter with the latest dyn-var values.
pub(crate) fn dynvar_overlay_snapshot() -> HashMap<String, Value> {
    REGEX_DYNVAR_OVERLAY.with(|slot| slot.borrow().clone().unwrap_or_default())
}

/// Publish a changed `$*` dynamic var into the overlay so subsequent subrule
/// pattern interpolation sees it. `name` is the env form (e.g. `*LEFT`).
pub(crate) fn dynvar_overlay_put(name: &str, value: Value) {
    REGEX_DYNVAR_OVERLAY.with(|slot| {
        if let Some(m) = slot.borrow_mut().as_mut() {
            m.insert(name.to_string(), value);
        }
    });
}

/// RAII guard that activates the reduce-time dyn-var overlay for the duration of
/// a grammar-actions parse and restores the previous state (overlay + seen flag)
/// on drop, so nested/re-entrant `Grammar.parse` calls stay balanced.
pub(crate) struct RegexDynvarOverlayGuard {
    prev_overlay: Option<HashMap<String, Value>>,
    prev_seen: bool,
}

impl RegexDynvarOverlayGuard {
    pub(crate) fn activate() -> Self {
        let prev_overlay =
            REGEX_DYNVAR_OVERLAY.with(|slot| slot.borrow_mut().replace(HashMap::default()));
        let prev_seen = REGEX_GRAMMAR_DYNVAR_SEEN.with(|c| c.replace(false));
        RegexDynvarOverlayGuard {
            prev_overlay,
            prev_seen,
        }
    }
}

impl Drop for RegexDynvarOverlayGuard {
    fn drop(&mut self) {
        REGEX_DYNVAR_OVERLAY.with(|slot| *slot.borrow_mut() = self.prev_overlay.take());
        REGEX_GRAMMAR_DYNVAR_SEEN.with(|c| c.set(self.prev_seen));
    }
}

/// Strip combining marks from a character, returning just the base character(s).
/// NFD-decompose the char and remove anything classified as a combining mark.
pub(super) fn strip_marks_char(ch: char) -> Vec<char> {
    ch.to_string()
        .nfd()
        .filter(|c| !is_combining_mark(*c))
        .collect()
}

/// Strip combining marks (and prepend characters) from text, working by
/// grapheme cluster.  Returns stripped base chars and a position map from
/// stripped index to original char index.  The sentinel for one-past-end is
/// also appended.
pub(crate) fn strip_marks_text(orig_chars: &[char]) -> (Vec<char>, Vec<usize>) {
    let text: String = orig_chars.iter().collect();
    let mut stripped_chars: Vec<char> = Vec::new();
    let mut pos_map: Vec<usize> = Vec::new(); // stripped idx -> original idx

    // Track the char-index offset as we iterate over grapheme clusters.
    let mut char_offset: usize = 0;
    for grapheme in text.graphemes(true) {
        let grapheme_start = char_offset;
        let grapheme_char_count = grapheme.chars().count();
        // NFD-decompose the entire grapheme and keep only non-combining-mark chars
        let bases: Vec<char> = grapheme.nfd().filter(|c| !is_combining_mark(*c)).collect();
        // Among the bases, drop Prepend characters and format characters (Cf)
        // that form part of multi-char grapheme clusters (e.g., ZWJ U+200D).
        // These are not the "base" letter of the grapheme.
        let is_multi_char = grapheme_char_count > 1;
        let filtered: Vec<char> = bases
            .into_iter()
            .filter(|c| !(is_prepend_char(*c) || is_multi_char && is_format_char(*c)))
            .collect();
        if filtered.is_empty() {
            // The entire grapheme is marks/format/prepends with no base — keep
            // the first non-combining char so the grapheme is not silently lost.
            for ch in grapheme.nfd() {
                if !is_combining_mark(ch) {
                    stripped_chars.push(ch);
                    pos_map.push(grapheme_start);
                    break;
                }
            }
        } else {
            for ch in filtered {
                stripped_chars.push(ch);
                pos_map.push(grapheme_start);
            }
        }
        char_offset += grapheme_char_count;
    }
    // sentinel for one-past-end
    pos_map.push(orig_chars.len());
    (stripped_chars, pos_map)
}

/// Check whether a character is a Unicode Prepend character (GCB=Prepend).
/// These are format characters that attach to the following character in a
/// grapheme cluster.
fn is_prepend_char(c: char) -> bool {
    matches!(c,
        '\u{0600}'..='\u{0605}'
        | '\u{06DD}'
        | '\u{070F}'
        | '\u{0890}'..='\u{0891}'
        | '\u{08E2}'
        | '\u{0D4E}'
        | '\u{110BD}'
        | '\u{110CD}'
        | '\u{111C2}'..='\u{111C3}'
        | '\u{1193F}'
        | '\u{11941}'
        | '\u{11A3A}'
        | '\u{11A84}'..='\u{11A89}'
        | '\u{11D46}'
    )
}

/// Check whether a character is a Unicode Format character (General_Category=Cf)
/// that commonly appears within grapheme clusters as a non-base element.
fn is_format_char(c: char) -> bool {
    matches!(c,
        '\u{00AD}'           // SOFT HYPHEN
        | '\u{200B}'         // ZERO WIDTH SPACE
        | '\u{200C}'         // ZERO WIDTH NON-JOINER
        | '\u{200D}'         // ZERO WIDTH JOINER
        | '\u{200E}'..='\u{200F}' // LRM, RLM
        | '\u{2060}'..='\u{2064}' // WORD JOINER, etc.
        | '\u{2066}'..='\u{2069}' // directional isolates
        | '\u{206A}'..='\u{206F}' // deprecated format chars
        | '\u{FEFF}'         // BOM / ZWNBSP
        | '\u{FE00}'..='\u{FE0F}' // Variation Selectors 1-16
        | '\u{E0100}'..='\u{E01EF}' // Variation Selectors 17-256
    )
}

/// Strip combining marks from all literal atoms in a RegexPattern (recursively),
/// memoized on the parsed pattern. A scoped `:ignoremark` token can be entered
/// once per candidate while scanning a document; keeping the derived tree on
/// the cached pattern removes that second per-invocation traversal as well.
pub(super) fn strip_marks_pattern(pattern: &RegexPattern) -> Arc<RegexPattern> {
    Arc::clone(
        pattern
            .derived
            .stripped
            .get_or_init(|| Arc::new(strip_marks_pattern_uncached(pattern))),
    )
}

fn strip_marks_pattern_uncached(pattern: &RegexPattern) -> RegexPattern {
    RegexPattern {
        tokens: pattern.tokens.iter().map(strip_marks_token).collect(),
        anchor_start: pattern.anchor_start,
        anchor_end: pattern.anchor_end,
        ignore_case: pattern.ignore_case,
        ignore_mark: false,
        derived: Default::default(),
    }
}

fn strip_marks_token(token: &RegexToken) -> RegexToken {
    RegexToken {
        atom: strip_marks_atom(&token.atom),
        quant: token.quant.clone(),
        named_capture: token.named_capture.clone(),
        secondary_named_capture: token.secondary_named_capture.clone(),
        hash_capture: token.hash_capture.clone(),
        force_list_capture: token.force_list_capture,
        ratchet: token.ratchet,
        frugal: token.frugal,
        separator: token.separator.as_ref().map(|s| {
            Box::new(RegexSeparatorSpec {
                pattern: strip_marks_pattern_uncached(&s.pattern),
                allow_trailing: s.allow_trailing,
            })
        }),
        from_runtime_interpolation: false,
    }
}

/// Wrap an already-parsed `RegexPattern` in a single-token pattern whose atom
/// is `RegexAtom::CaptureIsolatedGroup` — a sub-match that resolves its OWN
/// captures normally (so a backreference inside `pattern` to its own capture
/// still works — verified against real `raku`: `rx/ $<b>=(\w+) " " $<b> /`
/// invoked via `<$var>` still matches its repeated boundary), but does not
/// publish any positional or named capture into the caller's numbering (a
/// `<$var>`-family call gets its own discarded `Match` object in Raku). See
/// `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md` bug 2.
pub(crate) fn wrap_capture_isolated(pattern: RegexPattern) -> RegexPattern {
    let ignore_case = pattern.ignore_case;
    let ignore_mark = pattern.ignore_mark;
    RegexPattern {
        tokens: vec![RegexToken {
            atom: RegexAtom::CaptureIsolatedGroup(pattern),
            quant: RegexQuant::One,
            named_capture: None,
            secondary_named_capture: None,
            hash_capture: None,
            force_list_capture: false,
            ratchet: false,
            frugal: false,
            separator: None,
            from_runtime_interpolation: false,
        }],
        anchor_start: false,
        anchor_end: false,
        ignore_case,
        ignore_mark,
        derived: Default::default(),
    }
}

fn strip_marks_atom(atom: &RegexAtom) -> RegexAtom {
    match atom {
        RegexAtom::Literal(ch) => {
            let bases = strip_marks_char(*ch);
            // A precomposed char typically has exactly one base char
            if bases.len() == 1 {
                RegexAtom::Literal(bases[0])
            } else if bases.is_empty() {
                // Pure combining mark with no base — keep as-is
                RegexAtom::Literal(*ch)
            } else {
                // Multiple base chars (rare) — keep first
                RegexAtom::Literal(bases[0])
            }
        }
        RegexAtom::LiteralGrapheme(g) => {
            // `:ignoremark` compares base characters only, so a grapheme
            // literal collapses to its bases — usually a single one, which
            // becomes a plain `Literal` again.
            let stripped: String = g.nfd().filter(|c| !is_combining_mark(*c)).collect();
            let mut it = stripped.chars();
            match (it.next(), it.next()) {
                (Some(ch), None) => RegexAtom::Literal(ch),
                (Some(_), Some(_)) => RegexAtom::LiteralGrapheme(stripped.into()),
                _ => RegexAtom::LiteralGrapheme(g.clone()),
            }
        }
        RegexAtom::Named(name) => {
            // Named subrule / literal string match — strip marks from the name
            let stripped: String = name.nfd().filter(|c| !is_combining_mark(*c)).collect();
            RegexAtom::Named(stripped.into())
        }
        RegexAtom::Group(p) => RegexAtom::Group(strip_marks_pattern_uncached(p)),
        RegexAtom::CaptureGroup(p) => RegexAtom::CaptureGroup(strip_marks_pattern_uncached(p)),
        RegexAtom::CaptureIsolatedGroup(p) => {
            RegexAtom::CaptureIsolatedGroup(strip_marks_pattern_uncached(p))
        }
        RegexAtom::Alternation(alts) => {
            RegexAtom::Alternation(alts.iter().map(strip_marks_pattern_uncached).collect())
        }
        RegexAtom::SequentialAlternation(alts) => RegexAtom::SequentialAlternation(
            alts.iter().map(strip_marks_pattern_uncached).collect(),
        ),
        RegexAtom::Conjunction(branches) => {
            RegexAtom::Conjunction(branches.iter().map(strip_marks_pattern_uncached).collect())
        }
        RegexAtom::GoalMatch {
            goal,
            inner,
            goal_text,
        } => RegexAtom::GoalMatch {
            goal: strip_marks_pattern_uncached(goal),
            inner: strip_marks_pattern_uncached(inner),
            goal_text: goal_text.clone(),
        },
        RegexAtom::Lookaround {
            pattern,
            negated,
            is_behind,
        } => RegexAtom::Lookaround {
            pattern: strip_marks_pattern_uncached(pattern),
            negated: *negated,
            is_behind: *is_behind,
        },
        RegexAtom::CharClass(class) => RegexAtom::CharClass(strip_marks_char_class(class)),
        RegexAtom::CompositeClass { positive, negative } => RegexAtom::CompositeClass {
            positive: positive.iter().map(strip_marks_class_item).collect(),
            negative: negative.iter().map(strip_marks_class_item).collect(),
        },
        // All other atoms don't contain characters to strip
        other => other.clone(),
    }
}

fn strip_marks_char_class(class: &CharClass) -> CharClass {
    CharClass {
        negated: class.negated,
        items: class.items.iter().map(strip_marks_class_item).collect(),
    }
}

fn strip_marks_class_item(item: &ClassItem) -> ClassItem {
    match item {
        ClassItem::Char(ch) => {
            let bases = strip_marks_char(*ch);
            if bases.len() == 1 {
                ClassItem::Char(bases[0])
            } else {
                item.clone()
            }
        }
        ClassItem::Range(a, b) => {
            let a_bases = strip_marks_char(*a);
            let b_bases = strip_marks_char(*b);
            let new_a = if a_bases.len() == 1 { a_bases[0] } else { *a };
            let new_b = if b_bases.len() == 1 { b_bases[0] } else { *b };
            ClassItem::Range(new_a, new_b)
        }
        other => other.clone(),
    }
}

thread_local! {
    /// The subject of the innermost live engine invocation (ADR-0016 P3).
    /// Pushed by the public entry points around the engine walk; engine-side
    /// Match synthesis (reduce-time `$*` actions, `<?{ … }>` `.made`
    /// dispatch) reads the top, since capture accumulators carry no subject
    /// until the entry point publishes them. A stack because subrule argument
    /// evaluation can re-enter the public entry points mid-match.
    static CURRENT_MATCH_TARGET: RefCell<Vec<MatchTarget>> = const { RefCell::new(Vec::new()) };
}

/// RAII scope for [`CURRENT_MATCH_TARGET`].
pub(super) struct MatchTargetScope;

impl MatchTargetScope {
    pub(super) fn enter(target: MatchTarget) -> Self {
        CURRENT_MATCH_TARGET.with(|s| s.borrow_mut().push(target));
        MatchTargetScope
    }
}

impl Drop for MatchTargetScope {
    fn drop(&mut self) {
        CURRENT_MATCH_TARGET.with(|s| {
            s.borrow_mut().pop();
        });
    }
}

/// The innermost live engine subject, if any (see [`CURRENT_MATCH_TARGET`]).
pub(super) fn current_match_target() -> Option<MatchTarget> {
    CURRENT_MATCH_TARGET.with(|s| s.borrow().last().cloned())
}

/// Map a position from stripped char space back to original char space.
pub(super) fn map_pos(pos: usize, pos_map: &[usize], orig_len: usize) -> usize {
    if pos < pos_map.len() {
        pos_map[pos]
    } else {
        orig_len
    }
}

/// Remap every recorded span in a capture tree from a derived match space
/// (mark-stripped `:m` / case-folded `:i`) back to original char space
/// (ADR-0016 P3). Captured text derives from spans through the shared
/// subject, so spans recorded while matching a derived subject must be
/// translated before the captures are published — including sub-capture
/// nodes, which the pre-P3 code left in derived space (their stored text
/// papered over it).
pub(super) fn remap_caps_spans(caps: &mut RegexCaptures, pos_map: &[usize], orig_len: usize) {
    remap_caps_spans_offset(caps, pos_map, orig_len, 0);
}

/// [`remap_caps_spans`] with a base offset added after mapping — for the
/// engine-internal `:m` branch, which strips a mid-subject SLICE and must
/// land the spans back at the slice's absolute position. Does not touch
/// `from`/`to`/`capture_start`/`capture_end` when `offset != 0` (the caller
/// translates those itself; the accumulator's own span is not yet set there).
pub(super) fn remap_caps_spans_offset(
    caps: &mut RegexCaptures,
    pos_map: &[usize],
    orig_len: usize,
    offset: usize,
) {
    remap_caps_spans_mapped(caps, pos_map, orig_len, offset, 0);
}

/// Remap spans whose positions are relative to a suffix of a complete
/// mark-stripped subject. Unlike `offset`, `derived_offset` shifts the lookup
/// in the derived map; the resulting original positions are already absolute.
pub(super) fn remap_caps_spans_derived_offset(
    caps: &mut RegexCaptures,
    pos_map: &[usize],
    orig_len: usize,
    derived_offset: usize,
) {
    remap_caps_spans_mapped(caps, pos_map, orig_len, 0, derived_offset);
}

fn remap_caps_spans_mapped(
    caps: &mut RegexCaptures,
    pos_map: &[usize],
    orig_len: usize,
    offset: usize,
    derived_offset: usize,
) {
    let m = |p: usize| map_pos(p.saturating_add(derived_offset), pos_map, orig_len) + offset;
    if offset == 0 {
        caps.from = m(caps.from);
        caps.to = m(caps.to);
    }
    if !caps.positional_slots().is_empty() {
        for slot in caps.positional_slots_mut().iter_mut().flatten() {
            slot.0 = m(slot.0);
            slot.1 = m(slot.1);
        }
    }
    for sc in caps
        .named
        .values_mut()
        .flat_map(|slot| slot.nodes.iter_mut())
    {
        remap_cap_node_spans_mapped(
            std::sync::Arc::make_mut(sc),
            pos_map,
            orig_len,
            offset,
            derived_offset,
        );
    }
    for slot in caps.positional.iter_mut() {
        remap_pos_slot_mapped(slot, pos_map, orig_len, offset, derived_offset);
    }
}

/// Remap one positional slot's spans (its own, its subcap tree, and every
/// quantified iteration entry).
pub(super) fn remap_pos_slot(
    slot: &mut PosSlot,
    pos_map: &[usize],
    orig_len: usize,
    offset: usize,
) {
    remap_pos_slot_mapped(slot, pos_map, orig_len, offset, 0);
}

fn remap_pos_slot_mapped(
    slot: &mut PosSlot,
    pos_map: &[usize],
    orig_len: usize,
    offset: usize,
    derived_offset: usize,
) {
    let m = |p: usize| map_pos(p.saturating_add(derived_offset), pos_map, orig_len) + offset;
    slot.from = m(slot.from);
    slot.to = m(slot.to);
    if let Some(sc) = &mut slot.subcap {
        remap_cap_node_spans_mapped(
            std::sync::Arc::make_mut(sc),
            pos_map,
            orig_len,
            offset,
            derived_offset,
        );
    }
    for entry in slot.quantified.iter_mut().flatten() {
        entry.0 = m(entry.0);
        entry.1 = m(entry.1);
        if let Some(sc) = &mut entry.2 {
            remap_cap_node_spans_mapped(
                std::sync::Arc::make_mut(sc),
                pos_map,
                orig_len,
                offset,
                derived_offset,
            );
        }
    }
}

fn remap_cap_node_spans_mapped(
    node: &mut CapNode,
    pos_map: &[usize],
    orig_len: usize,
    offset: usize,
    derived_offset: usize,
) {
    let m = |p: usize| map_pos(p.saturating_add(derived_offset), pos_map, orig_len) + offset;
    node.from = m(node.from);
    node.to = m(node.to);
    let Some(children) = node.children.as_deref_mut() else {
        return;
    };
    for sc in children
        .named
        .values_mut()
        .flat_map(|slot| slot.nodes.iter_mut())
    {
        remap_cap_node_spans_mapped(
            std::sync::Arc::make_mut(sc),
            pos_map,
            orig_len,
            offset,
            derived_offset,
        );
    }
    for slot in children.positional.iter_mut() {
        remap_pos_slot_mapped(slot, pos_map, orig_len, offset, derived_offset);
    }
}

/// Iterator that yields all case variants of a character (lowercase + uppercase + titlecase).
/// Deduplicates so each distinct char is yielded at most once.
pub(super) struct CaseFoldIter {
    chars: Vec<char>,
    idx: usize,
}

impl CaseFoldIter {
    pub(super) fn new(c: char) -> Self {
        let mut chars = Vec::with_capacity(4);
        chars.push(c);
        for lc in c.to_lowercase() {
            if !chars.contains(&lc) {
                chars.push(lc);
            }
        }
        for uc in c.to_uppercase() {
            if !chars.contains(&uc) {
                chars.push(uc);
            }
        }
        CaseFoldIter { chars, idx: 0 }
    }
}

impl Iterator for CaseFoldIter {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        if self.idx < self.chars.len() {
            let c = self.chars[self.idx];
            self.idx += 1;
            Some(c)
        } else {
            None
        }
    }
}

pub(crate) struct NamedRegexLookupSpec {
    pub(crate) silent: bool,
    pub(crate) token_lookup: bool,
    pub(crate) lookup_name: String,
    /// [`Self::lookup_name`] interned. The spec itself is memoized per atom
    /// text, so this interns once per distinct `<subrule>` atom in the program
    /// rather than once per call — the left-recursion key is built from it on
    /// every subrule call at every position
    /// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
    pub(crate) lookup_sym: crate::symbol::Symbol,
    pub(crate) capture_name: Option<String>,
    /// [`Self::capture_name`] interned, `None` when the atom carries no alias.
    /// Interned with the spec (once per distinct `<subrule>` atom) rather than
    /// per candidate: `build_named_candidates_from_inner` files every matched
    /// subrule under this name, so interning it there re-hashed the same string
    /// once per capture -- 37,012 interns on a 60-row YAML parse
    /// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
    pub(crate) capture_sym: Option<crate::symbol::Symbol>,
    pub(crate) arg_exprs: Vec<String>,
    /// When true, the alias replaces the original capture name (dot-call alias).
    /// `<foo=.alpha>` sets this to true; `<foo=alpha>` leaves it false.
    pub(crate) alias_replaces_original: bool,
}

/// Check if a character is a "word" character for word boundary purposes.
/// In Raku, word characters are alphanumeric or underscore.
pub(super) fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Check if a CharClass contains only exact character items (Char and Range).
/// Such classes should NOT match grapheme clusters with combining marks,
/// because in Raku, `<[Dd]>` matches the grapheme "D" but not "D + combiners".
/// Property-based classes like `\w` match on the base character regardless.
pub(super) fn class_has_only_exact_chars(class: &CharClass) -> bool {
    class
        .items
        .iter()
        .all(|item| matches!(item, ClassItem::Char(_) | ClassItem::Range(_, _)))
}

/// Advance past a single grapheme cluster starting at `pos` in `chars`.
/// After matching the base character at `pos`, this skips any trailing
/// combining marks (Unicode category M) so that a single regex atom
/// consumes the full grapheme, matching Raku's grapheme-level semantics.
pub(crate) fn grapheme_end(chars: &[char], pos: usize) -> usize {
    // Fast path: no ASCII codepoint is a combining mark, so a non-control
    // ASCII base followed by ASCII (or by nothing) is a cluster of its own and
    // the general path below would walk zero marks to say so. This is the
    // whole of ordinary text, and it is on the per-character reject path of
    // every scan, so the two table lookups it skips are worth an explicit
    // branch (#8450). Controls -- `\r` above all, whose CRLF pair is handled
    // just below -- are excluded and fall through.
    if let Some(&c) = chars.get(pos)
        && c.is_ascii()
        && !c.is_ascii_control()
        && chars.get(pos + 1).is_none_or(|next| next.is_ascii())
    {
        return pos + 1;
    }
    // \r\n is a single grapheme cluster in Raku
    if pos < chars.len() && chars[pos] == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
        return pos + 2;
    }
    // UAX #29 GB4: nothing extends a control character, so its cluster is
    // itself even when a combining mark follows (`"\t\x[0300]"` is two
    // graphemes, and `/\t/` matches its first).
    if pos < chars.len() && is_grapheme_control(chars[pos]) {
        return pos + 1;
    }
    let mut end = pos + 1;
    let mut saw_linker = false;
    while end < chars.len() && is_combining_mark(chars[end]) {
        saw_linker |= is_conjunct_linker(chars[end]);
        end += 1;
    }
    if saw_linker && end < chars.len() {
        // An Indic virama does not end its cluster: UAX #29's GB9c joins the
        // consonant that follows it, so `क` + `्` + `ष` is the single grapheme
        // `क्ष`. Only the real segmentation knows whether that rule applies
        // here (it does not for `a` + `्` + `b`, where neither side is an
        // Indic consonant), so pay for it on the rare linker-bearing cluster.
        return uax29_grapheme_end(chars, pos);
    }
    end
}

/// True for an Indic virama / conjunct linker — canonical combining class 9.
fn is_conjunct_linker(c: char) -> bool {
    unicode_normalization::char::canonical_combining_class(c) == 9
}

/// True for a codepoint UAX #29 treats as `Control` (plus the line/paragraph
/// separators): a cluster boundary always follows one, whatever comes next.
fn is_grapheme_control(c: char) -> bool {
    c.is_control() || c == '\u{2028}' || c == '\u{2029}'
}

/// The end of the grapheme cluster at `pos` under the full UAX #29 rules.
///
/// Segmentation needs a `&str`, so it runs over a window of the remaining
/// codepoints that is grown until the first cluster ends inside it (a cluster
/// that fills the whole window may have been cut short by the window itself).
fn uax29_grapheme_end(chars: &[char], pos: usize) -> usize {
    let remaining = chars.len() - pos;
    let mut window = 16;
    loop {
        let take = window.min(remaining);
        let text: String = chars[pos..pos + take].iter().collect();
        let first = text
            .graphemes(true)
            .next()
            .map_or(1, |g| g.chars().count())
            .max(1);
        if first < take || take == remaining {
            return pos + first;
        }
        window *= 2;
    }
}

/// True when `pos` sits on a grapheme-cluster boundary of `chars`.
///
/// Raku strings are NFG, so a regex atom can only start where a grapheme
/// starts: `"a\x[094D]b" ~~ /\x[094D]/` is `False` in Rakudo because the mark
/// is *inside* the first grapheme, never an atom of its own. The common case
/// (neither this codepoint nor the one before it can extend a cluster) costs
/// two array reads.
pub(super) fn is_grapheme_boundary(chars: &[char], pos: usize) -> bool {
    if pos == 0 || pos >= chars.len() {
        return true;
    }
    if chars[pos] == '\n' && chars[pos - 1] == '\r' {
        return false;
    }
    // Fast path, for the same reason as `grapheme_end`'s: neither an ASCII
    // codepoint nor an ASCII predecessor can be a combining mark, so the
    // mark-run walk below would find an empty run and return `true`. CRLF is
    // the one ASCII pair that is not a boundary, and it was just ruled out.
    if chars[pos].is_ascii() && chars[pos - 1].is_ascii() {
        return true;
    }
    // Find the start of the combining-mark run this position sits in or after.
    let mut run_start = pos;
    while run_start > 0 && is_combining_mark(chars[run_start - 1]) {
        run_start -= 1;
    }
    if is_combining_mark(chars[pos]) {
        // Only the first mark of a run can begin a cluster, and only when
        // nothing before it can be extended (GB4 — after a control, or at the
        // start of the string, a mark stands alone).
        return run_start == pos && run_start > 0 && is_grapheme_control(chars[run_start - 1]);
    }
    if run_start == pos || run_start == 0 {
        return true;
    }
    // A conjunct linker in the mark run just before `pos` can pull this
    // codepoint into the preceding cluster (GB9c, as in `grapheme_end`).
    grapheme_end(chars, run_start - 1) <= pos
}

/// Check if an atom is "simple" — it only advances position without producing
/// any captures. Used to enable a fast path in ratcheted quantifier loops
/// that avoids cloning RegexCaptures on every iteration.
/// Returns true when a Named regex atom is "silent" (produces no implicit
/// named capture).  Silent names start with `.` (e.g. `<.ws>`).
pub(super) fn is_silent_named_atom(atom: &RegexAtom) -> bool {
    if let RegexAtom::Named(name) = atom {
        name.trim().starts_with('.')
    } else {
        false
    }
}

/// Check if a Named atom is non-silent (produces named captures) and has no arguments.
/// Such atoms can use a fast path for ratcheted quantifiers.
pub(super) fn is_named_atom_no_args(atom: &RegexAtom) -> bool {
    if let RegexAtom::Named(name) = atom {
        let trimmed = name.trim();
        !trimmed.starts_with('.')
            && !trimmed.starts_with('&')
            && !trimmed.contains('(')
            && !trimmed.contains(':')
            && !trimmed.contains('=')
    } else {
        false
    }
}

pub(super) fn is_simple_atom(atom: &RegexAtom) -> bool {
    matches!(
        atom,
        RegexAtom::Literal(_)
            | RegexAtom::LiteralGrapheme(_)
            | RegexAtom::CharClass(_)
            | RegexAtom::Any
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::UnicodeProp { .. }
            | RegexAtom::CompositeClass { .. }
    )
}

pub(super) fn merge_regex_captures(
    mut dst: RegexCaptures,
    mut src: RegexCaptures,
) -> RegexCaptures {
    for (k, v) in src.named.drain() {
        dst.named.entry(k).or_default().merge(v);
    }
    dst.extend_capture_alias_map(src.take_capture_alias_map());
    dst.positional.append(&mut src.positional);
    // Writes an inline `{ … }` made to the regex's own `:my`/`:let` lexicals are
    // in the same lexical scope as the level being merged into (this helper only
    // folds inline sub-patterns — a conjunction branch, a `~` goal), so they come
    // with it. Without this a `[ … { $x = … } … ]` inside one of those shapes
    // silently lost the write.
    dst.extend_regex_vars(src.take_regex_vars());
    dst.merge_hash_captures(src.take_hash_captures());
    // Propagate `<(` / `)>` capture-marker positions from the merged-in side so a
    // sub-pattern that sets the match boundaries is not lost when its captures are
    // folded into an outer result — e.g. `'<' ~ '>' [<( \w+ )>]`, where the
    // goalpost merges the inner (marker-carrying) captures into the goal's.
    if src.capture_start.is_some() {
        dst.capture_start = src.capture_start;
    }
    if src.capture_end.is_some() {
        dst.capture_end = src.capture_end;
    }
    adopt_inline_ast(&mut dst, &mut src);
    dst
}

/// Carry an inline `{ make … }` value from a sub-pattern's captures up to the
/// level they are being folded into. A `make` belongs to the enclosing *rule*
/// node, so it travels with the sub-pattern's code blocks — wherever a caller
/// keeps `src`'s blocks on their own capture node instead (a subrule match),
/// the value stays there too and must NOT be adopted.
pub(super) fn adopt_inline_ast(dst: &mut RegexCaptures, src: &mut RegexCaptures) {
    if src.ast.is_some() {
        dst.ast = src.ast.take();
    }
}

/// Count how many positional capture groups the given atom will produce.
pub(super) fn count_capture_groups(atom: &RegexAtom) -> usize {
    match atom {
        RegexAtom::CaptureGroup(_) => 1,
        RegexAtom::Group(pat) => count_pattern_capture_groups(pat),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            // All alternatives should produce the same number of captures
            alts.iter()
                .map(count_pattern_capture_groups)
                .max()
                .unwrap_or(0)
        }
        _ => 0,
    }
}

/// The positional-capture width reserved by an alternation.  Capture numbers
/// after an alternation follow its widest branch, so a shorter winning branch
/// must contribute Nil slots for the captures it did not take.
pub(super) fn alternation_capture_slots(alts: &[RegexPattern]) -> usize {
    alts.iter()
        .map(count_pattern_capture_groups)
        .max()
        .unwrap_or(0)
}

/// For each positional slot `atom` will produce (same order and count as
/// [`count_capture_groups`]), whether raku renders it as an empty LIST
/// (`[]`) rather than `Nil` when the enclosing `?` token takes its zero
/// branch and this slot is never actually matched.
///
/// A slot is list-shaped when the token that owns it sits under a list
/// quantifier (`*`/`+`/`**`/a `%` separator) at or above itself — mirrors
/// `collect_nested_list_quantified_names`'s identical walk for NAMED
/// captures (`(a)?(b)` on "b" yields `$0 = Nil`, but `[ (a)+ ]?` on ""
/// yields `$0 = []`, same as a zero-iteration `(a)*`). `ambient_list` carries
/// whether an enclosing token in the walk was already list-quantified.
pub(super) fn capture_group_list_flags(atom: &RegexAtom, ambient_list: bool) -> Vec<bool> {
    match atom {
        RegexAtom::CaptureGroup(_) => vec![ambient_list],
        RegexAtom::Group(pat) => pattern_capture_group_list_flags(pat, ambient_list),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => alts
            .iter()
            .map(|p| pattern_capture_group_list_flags(p, ambient_list))
            .max_by_key(|flags| flags.len())
            .unwrap_or_default(),
        _ => Vec::new(),
    }
}

fn pattern_capture_group_list_flags(pat: &RegexPattern, ambient_list: bool) -> Vec<bool> {
    let mut out = Vec::new();
    for token in &pat.tokens {
        let token_is_list = ambient_list
            || matches!(
                token.quant,
                RegexQuant::ZeroOrMore
                    | RegexQuant::OneOrMore
                    | RegexQuant::Repeat(..)
                    | RegexQuant::RepeatCode(_)
            )
            || token.separator.is_some();
        out.extend(capture_group_list_flags(&token.atom, token_is_list));
    }
    out
}

/// Whether matching `atom` involves an alternation whose branches can have
/// different lengths — the case where a greedy quantifier (`*`/`+`/`**`) must be
/// able to backtrack into a *shorter* per-iteration choice to satisfy a later
/// constraint (e.g. `(a | b | bc | cde)+»`). Used to gate the (more expensive)
/// full backtracking quantifier expansion so simple atoms keep the fast greedy
/// chain.
pub(super) fn atom_contains_alternation(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            alts.len() > 1 || alts.iter().any(pattern_contains_alternation)
        }
        RegexAtom::Group(pat) | RegexAtom::CaptureGroup(pat) => pattern_contains_alternation(pat),
        _ => false,
    }
}

fn pattern_contains_alternation(pat: &RegexPattern) -> bool {
    pat.tokens
        .iter()
        .any(|t| atom_contains_alternation(&t.atom))
}

/// Count positional capture groups in a pattern (non-recursive into nested groups).
fn count_pattern_capture_groups(pat: &RegexPattern) -> usize {
    *pat.derived.capture_group_count.get_or_init(|| {
        let mut count = 0;
        for token in &pat.tokens {
            count += count_capture_groups(&token.atom);
        }
        count
    })
}

/// Fold quantified captures. After a quantifier loop, positional entries from
/// `base_len` onward may contain repeated captures from multiple iterations.
/// If `stride` > 0 (captures per iteration), fold them into quantified lists.
pub(super) fn fold_quantified_captures(
    caps: &mut RegexCaptures,
    base_len: usize,
    stride: usize,
    fold_single: bool,
) {
    if stride == 0 {
        return;
    }
    let new_entries = caps.positional.len() - base_len;
    if new_entries == 0 {
        // A zero-iteration `*` / `**0..` quantified capture group reserves an
        // empty-list slot per inner capture (Raku: `(z)*` matching 0 times yields
        // `[]`). Index stability is preserved because an earlier unmatched `(...)?`
        // reserves its own Nil slot (see `reserve_nil_capture_slots`), so both
        // `(y)?`→Nil and `(z)*`→[] coexist at their correct positions.
        for _ in 0..stride {
            caps.positional.push(PosSlot {
                quantified: Some(Vec::new()),
                ..Default::default()
            });
        }
        return;
    }
    if new_entries < stride {
        // Fewer captures than one iteration's worth: malformed, leave as is.
        return;
    }
    if new_entries == stride && !fold_single {
        // A bare `?` is the one quantifier Raku does NOT listify: `(a)?` binds
        // `$0` to a Match (or Nil), unlike `(a)**0..1`, which yields a
        // one-element Array. Every other quantifier folds even a single
        // iteration (`fold_single`).
        return;
    }
    let iterations = new_entries / stride;
    if iterations * stride != new_entries {
        // Uneven — don't fold (shouldn't happen with well-formed captures)
        return;
    }

    // Collect entries per group; the last iteration's span/subcap become the
    // folded slot's "representative" values for backref purposes.
    let mut folded: Vec<PosSlot> = Vec::with_capacity(stride);
    for group in 0..stride {
        let mut list: Vec<QuantifiedCaptureEntry> = Vec::with_capacity(iterations);
        for iter in 0..iterations {
            let idx = base_len + iter * stride + group;
            let slot = &caps.positional[idx];
            list.push((slot.from, slot.to, slot.subcap.clone()));
        }
        let last = list.last().unwrap();
        folded.push(PosSlot {
            from: last.0,
            to: last.1,
            subcap: last.2.clone(),
            quantified: Some(list),
            nil: false,
            alternation_padding: false,
        });
    }

    // Replace entries from base_len onward
    caps.positional.truncate(base_len);
    caps.positional.extend(folded);
}

/// Materialize the positional slots' texts through the engine's subject
/// (ADR-0016 P4), from the same `chars` the spans were recorded against, so the
/// semantics match the pre-P4 stored-text axis exactly.
pub(super) fn pos_slot_texts(slots: &[PosSlot], chars: &[char]) -> Vec<String> {
    slots
        .iter()
        .map(|slot| span_chars_text(slot.from, slot.to, chars))
        .collect()
}

/// [`pos_slot_texts`] for the named axis: the per-name text lists. Silent-action
/// marker keys never had text entries pre-P4 and are skipped.
pub(super) fn named_slot_texts(
    named: &crate::runtime::NamedCaptureMap,
    chars: &[char],
) -> std::collections::HashMap<String, Vec<String>> {
    named
        .iter()
        .filter(|(k, _)| !k.starts_with(SILENT_ACTION_MARKER_PREFIX))
        .map(|(k, slot)| {
            (
                k.resolve(),
                slot.nodes
                    .iter()
                    .map(|n| span_chars_text(n.from, n.to, chars))
                    .collect(),
            )
        })
        .collect()
}

/// One span's text through a chars slice, clamped.
fn span_chars_text(from: usize, to: usize, chars: &[char]) -> String {
    let from = from.min(chars.len());
    let to = to.min(chars.len()).max(from);
    chars[from..to].iter().collect()
}

/// Reserve one index-stable positional slot per entry of `flags` for an
/// unmatched optional capture group (`(x)?` that matched zero times, or a
/// whole `[ ... ]?` that took its zero branch). A `false` entry renders as
/// `Nil` in the resulting Match (Raku: `(a)?(b)` on "b" yields `$0 = Nil`,
/// `$1 = b`); a `true` entry (the group sits under a list quantifier — see
/// [`capture_group_list_flags`]) renders as an empty list, same as a
/// zero-iteration `(a)*`.
pub(super) fn reserve_nil_capture_slots(caps: &mut RegexCaptures, flags: &[bool]) {
    if flags.is_empty() {
        return;
    }
    let at = caps.to;
    for &is_list in flags {
        caps.positional.push(if is_list {
            PosSlot {
                from: at,
                to: at,
                quantified: Some(Vec::new()),
                ..Default::default()
            }
        } else {
            PosSlot {
                from: at,
                to: at,
                nil: true,
                ..Default::default()
            }
        });
    }
}

/// Check if a character matches a named builtin character class.
pub(super) fn matches_named_builtin(name: &str, c: char) -> bool {
    match name {
        "alpha" => c.is_alphabetic() || c == '_',
        "upper" => check_unicode_property("Uppercase_Letter", c),
        "lower" => check_unicode_property("Lowercase_Letter", c),
        "digit" => c.is_ascii_digit(),
        "xdigit" => c.is_ascii_hexdigit(),
        "space" | "ws" => c.is_whitespace(),
        // In a character-class context `<ident>` denotes one identifier-start
        // character.  The sequence form (`<ident>` outside a class) is lowered
        // separately by the regex parser to alpha followed by alnum*.
        "ident" => c.is_alphabetic() || c == '_',
        "alnum" => c.is_alphabetic() || c == '_' || c.is_ascii_digit(),
        "blank" => c == '\t' || c == ' ' || c == '\u{A0}',
        "cntrl" => c.is_control(),
        "punct" => check_unicode_property("Punctuation", c),
        "graph" => {
            // Raku's POSIX `graph` is Letters ∪ decimal digits (Nd) ∪ Punctuation.
            // It EXCLUDES the Symbol categories (Sm/Sc/Sk/So — `^ $ ~ + = < > | °`),
            // Marks, and non-decimal Number categories (No/Nl) — narrower than the
            // C/POSIX "any visible char". Matching raku is required so `<+graph
            // -punct>` (a common "word char" idiom) does not swallow `^`/`$`/…; e.g.
            // Template::Mustache's grammar `token ident { <+ graph - punct> ... }`
            // must reject the `^`/`$`/`<` sigils so `{{^inverted}}` / `{{<parent}}`
            // tags dispatch to the section rule instead of the plain-var rule.
            check_unicode_property("Letter", c)
                || c == '_'
                || check_unicode_property("Nd", c)
                || check_unicode_property("Punctuation", c)
        }
        "print" => {
            // print: graph + space-like characters (but not control characters)
            !c.is_control()
        }
        _ => false,
    }
}
