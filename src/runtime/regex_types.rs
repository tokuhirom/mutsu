//! Data types for the regex engine — the parsed-pattern representation
//! (`RegexPattern`/`RegexToken`/`RegexAtom`/…) and the character-class model
//! (`CharClass`/`ClassItem`). The capture types (`RegexCaptures`, `CapNode`,
//! `PosSlot`, …) live in `value::regex_caps` and are re-exported here.
//!
//! Extracted verbatim from `runtime/mod.rs` (2026-07-21 hygiene re-slim). These
//! were parent-module-private structs whose fields the sibling `regex`/
//! `regex_parse*` modules read directly; moving them into their own module
//! widens the previously module-private structs and fields to `pub(crate)` so
//! those siblings keep their access (the whole set is re-exported from
//! `runtime` via `pub(crate) use self::regex_types::*`).

pub(crate) use crate::value::regex_caps::{
    CapNode, OuterBackrefCaps, PosSlot, QuantifiedCaptureEntry, RegexCaptures, RegexVarMap,
    SILENT_ACTION_MARKER_PREFIX,
};
use rustc_hash::FxHashMap as HashMap;
use std::sync::Arc;

#[derive(Clone)]
pub(crate) struct RegexPattern {
    pub(crate) tokens: Vec<RegexToken>,
    pub(crate) anchor_start: bool,
    pub(crate) anchor_end: bool,
    pub(crate) ignore_case: bool,
    pub(crate) ignore_mark: bool,
    /// Analyses derived from this pattern, each computed at most once and
    /// shared by every holder of the pattern. See [`PatternDerived`].
    pub(crate) derived: Arc<PatternDerived>,
}

/// The lazily-derived, pure-function-of-the-pattern analyses that hang off a
/// [`RegexPattern`].
///
/// Static parsed patterns are shared through the regex parse cache, so a
/// derivation paid here is paid once for every match that pattern will ever
/// take part in — which is what makes the scan prefilter (ADR-0099 Stage 1)
/// affordable at all: its first-set derivation walks the whole token tree and
/// evaluates every character class over the ASCII range, far too much to
/// repeat per `regex_scan_positions` call.
#[derive(Default)]
pub(crate) struct PatternDerived {
    /// Mark-stripped form, for scoped `:ignoremark` (which can enter the same
    /// pattern many times during one match).
    pub(crate) stripped: std::sync::OnceLock<Arc<RegexPattern>>,
    /// Case-folded form, for a whole-pattern `:i` over a subject or pattern
    /// with a multi-character fold (matched on the folded subject), so its
    /// compiled program is built once rather than per match.
    pub(crate) folded: std::sync::OnceLock<Arc<RegexPattern>>,
    /// The unanchored-scan prefilter (ADR-0099 Stage 1), for a pattern that
    /// mentions no rule name — a pure function of the pattern, so one slot.
    pub(crate) prefilter:
        std::sync::OnceLock<Arc<crate::runtime::regex::regex_prefilter::Prefilter>>,
    /// Whether the pattern mentions a `<subrule>` anywhere, which is what
    /// decides between the two memos above and below. Derived once because it
    /// is asked on every scan.
    pub(crate) mentions_subrule: std::sync::OnceLock<bool>,
    /// The same prefilter for a pattern that DOES mention a rule name, where
    /// the derivation is not a pure function of the pattern: the same name
    /// resolves to different bodies in different packages (`grammar H is G`
    /// overriding `token x`) and to different bodies after any (re)definition.
    /// So the entries are keyed by both, exactly as ADR-0099 §4 constraint 3
    /// requires — see the `regex_prefilter_subrule` module (private to
    /// `runtime::regex`, so not linkable from here).
    ///
    /// A short vector rather than a map: one pattern is scanned from a handful
    /// of packages at most, and a stale `TOKEN_DEFS_GEN` clears the lot.
    pub(crate) prefilter_in_pkg: std::sync::Mutex<Vec<PkgPrefilter>>,
    /// Whether this pattern's subtree contains a backreference anywhere
    /// inside it (`atom_contains_backref`'s per-pattern memo). A pure
    /// function of the pattern shape, asked on every match/backtrack attempt
    /// of the atom that owns this pattern (once for every `<?{ }>`-gated
    /// backtrack retry in #8510's shape), so a full re-walk per call is pure
    /// waste once this pattern's own answer is known.
    pub(crate) contains_backref: std::sync::OnceLock<bool>,
    /// Whether this pattern's subtree contains a code atom at its own capture
    /// level (`atom_contains_code`'s per-pattern memo).
    pub(crate) contains_code: std::sync::OnceLock<bool>,
    /// This pattern's own positional-capture-group count (`count_capture_groups`'s
    /// per-pattern memo). Also a pure function of the pattern shape, re-walked
    /// on every group match otherwise — same #8510 backtrack-retry cost shape
    /// as `contains_backref` above.
    pub(crate) capture_group_count: std::sync::OnceLock<usize>,
    /// This pattern's own capture-name multiplicity map, entered with no
    /// ambient list context (`pattern_name_mult`'s per-pattern memo, used to
    /// seed an untaken `|` branch's list-valued names as empty lists rather
    /// than leaving them absent — #9675). A pure function of the pattern
    /// shape, otherwise re-walked, and every `$<name>=` alias re-interned, on
    /// every alternation-branch attempt — same #8510 backtrack-retry cost
    /// shape as `contains_backref` above.
    pub(crate) name_mult: std::sync::OnceLock<
        Arc<HashMap<crate::symbol::Symbol, crate::runtime::regex::regex_helpers::NameMult>>,
    >,
    /// The declarative-prefix NFA of this pattern as a `|` branch (ADR-0125),
    /// one entry per package it was ranked from. Building one resolves rule
    /// names, so, like `prefilter_in_pkg`, the entries are keyed by package
    /// and `TOKEN_DEFS_GEN`.
    pub(crate) ltm_nfa: crate::runtime::regex::regex_ltm_nfa::LtmNfaSlots,
    /// The pattern compiled to a flat backtracking program (ADR-0135), or
    /// `None` when it holds a construct the compiled engine does not cover
    /// yet and keeps the tree walk. A pure function of the pattern: a subrule
    /// call is resolved when it is reached, not when the pattern compiles.
    pub(crate) rx_program: std::sync::OnceLock<Option<Arc<crate::runtime::regex::RxProgram>>>,
}

/// One package's entry in [`PatternDerived::prefilter_in_pkg`].
pub(crate) struct PkgPrefilter {
    pub(crate) pkg: crate::symbol::Symbol,
    pub(crate) token_defs_gen: u64,
    pub(crate) prefilter: Arc<crate::runtime::regex::regex_prefilter::Prefilter>,
}

#[derive(Clone)]
pub(crate) struct RegexToken {
    pub(crate) atom: RegexAtom,
    pub(crate) quant: RegexQuant,
    pub(crate) named_capture: Option<String>,
    /// Secondary named capture for capturing subrule aliases like `$<alias>=<builtin_class>`.
    /// When set, the matched text is also stored under this name (the original rule name).
    pub(crate) secondary_named_capture: Option<String>,
    /// Hash aliasing: `%<name>=(...)` captures build a hash
    pub(crate) hash_capture: Option<String>,
    /// Array-sigil capture alias (`@<name>=(...)`): forces the named capture
    /// into list context, so even a single (non-quantified) match yields a
    /// one-element List rather than a bare Match. Mirrors Raku's `@`-sigil
    /// declaration semantics for hypothetical capture variables.
    pub(crate) force_list_capture: bool,
    pub(crate) ratchet: bool,
    /// Frugal (non-greedy) quantifier modifier: `*?`, `+?`, `??`
    pub(crate) frugal: bool,
    /// Separator for `%` / `%%` quantifiers, e.g. `<thing> +% ','`. When present,
    /// the quantified atom is matched with `separator.atom` interleaved between
    /// iterations. `allow_trailing` is true for `%%` (an optional trailing
    /// separator is permitted). The separator's own captures are appended as
    /// positional/named captures after the main atom's, matching Raku semantics.
    pub(crate) separator: Option<Box<RegexSeparatorSpec>>,
    /// ADR-0022 Slice 5: true when this token's `RegexAtom::Literal` char
    /// came from interpolating a *non-constant* runtime variable's value
    /// into the pattern text (`interpolate_regex_scalars`), as opposed to a
    /// literal character written directly in the source or interpolated
    /// from a `constant`-declared value (which Rakudo inlines at compile
    /// time and so still participates in LTM ranking like a hand-written
    /// literal — ADR-0022 §2's "non-constant `$var` interpolation" row).
    /// `ltm_atom_mode`'s callers and `regex_ltm_litend` treat a token with this
    /// set as a `Terminate` stopper: it neither extends the declarative
    /// prefix nor contributes to litlen. Always `false` outside
    /// `LTM_DECLARATIVE_MODE` measurement — it does not affect ordinary
    /// matching at all.
    pub(crate) from_runtime_interpolation: bool,
    /// True when this token's named capture comes from a subrule CALL (a
    /// builtin subrule such as `<alpha>` or `<after x>`, bare or under a
    /// sigil alias `$<a>=<alpha>`), not from a user alias on a plain atom
    /// (`$<x>=<[cd]>`). A call that a `?` skips never ran, so it publishes
    /// no capture; an aliased plain atom still renders an empty Match
    /// (#9212).
    pub(crate) subrule_call_capture: bool,
}

#[derive(Clone)]
pub(crate) struct RegexSeparatorSpec {
    /// The separator sub-pattern (matched between iterations). Holding a full
    /// pattern preserves named captures, quantifiers, and other structure of
    /// complex separators such as `$<delim>=<[a..z]>*`.
    pub(crate) pattern: RegexPattern,
    pub(crate) allow_trailing: bool,
}

/// A `<subrule>` atom's written text, with its parsed lookup spec memoized
/// alongside it.
///
/// The spec (silent/token flavour, alias, interned names, argument
/// expressions) is a pure function of the text, and the text is fixed once the
/// pattern is parsed — but the matcher asked for it per *call*, through a
/// process-wide `text -> spec` map: 113,480 probes and 1.35% of a 60-row
/// YAMLish parse, all of them re-deriving what one node had already answered
/// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)). Answering it from
/// the node costs one already-initialized `OnceLock` read.
///
/// The first read still goes through
/// [`Interpreter::parse_named_regex_lookup_spec`](crate::runtime::Interpreter),
/// so two atoms spelled the same share one `Arc` exactly as before.
#[derive(Clone, Default)]
pub(crate) struct NamedAtom {
    text: String,
    spec: std::sync::OnceLock<Arc<crate::runtime::regex::regex_helpers::NamedRegexLookupSpec>>,
}

impl NamedAtom {
    /// This atom's parsed lookup spec, derived once.
    #[inline]
    pub(crate) fn spec(&self) -> &Arc<crate::runtime::regex::regex_helpers::NamedRegexLookupSpec> {
        self.spec
            .get_or_init(|| crate::runtime::Interpreter::parse_named_regex_lookup_spec(&self.text))
    }
}

impl From<String> for NamedAtom {
    fn from(text: String) -> Self {
        NamedAtom {
            text,
            spec: std::sync::OnceLock::new(),
        }
    }
}

impl From<&str> for NamedAtom {
    fn from(text: &str) -> Self {
        NamedAtom::from(text.to_string())
    }
}

impl std::ops::Deref for NamedAtom {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        &self.text
    }
}

impl std::fmt::Display for NamedAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.text)
    }
}

impl PartialEq<str> for NamedAtom {
    fn eq(&self, other: &str) -> bool {
        self.text == other
    }
}

impl PartialEq<&str> for NamedAtom {
    fn eq(&self, other: &&str) -> bool {
        self.text == *other
    }
}

#[derive(Clone)]
pub(crate) enum RegexAtom {
    Literal(char),
    /// A literal *grapheme* that spans more than one codepoint, e.g. the
    /// Devanagari cluster `क्ष` (`क` U+0915, virama U+094D, `ष` U+0937) or a
    /// base character followed by combining marks that NFC cannot compose
    /// away. Raku's regex grammar works on graphemes, so such a cluster is a
    /// single atom: the tokenizer's per-codepoint scan is re-joined by
    /// `merge_grapheme_literal_tokens`, and matching it consumes the whole
    /// grapheme rather than its leading codepoint.
    LiteralGrapheme(Box<str>),
    Named(NamedAtom),
    Any,
    CharClass(CharClass),
    /// `<.ws>` — Raku's word-boundary-aware whitespace rule:
    /// requires `\s+` between word characters, `\s*` otherwise.
    WsRule,
    Newline,
    NotNewline,
    Group(RegexPattern),
    CaptureGroup(RegexPattern),
    /// A capture-isolated sub-match: matches `RegexPattern` exactly like
    /// `Group` (its own nested captures resolve normally, so a
    /// backreference WITHIN it to its OWN capture still works), but its
    /// positional/named captures are never merged into the caller's numbering
    /// — the caller sees only the matched extent, like a discarded `Match`
    /// object. Used for the `<$var>` / bare-`$var` / `<@var>`-alternation
    /// "stored Regex value" family: Raku gives such a sub-match its own
    /// discarded `Match`, so its captures must not leak into the outer `$/`
    /// (see `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`
    /// bug 2, and the Cro::HTTP `http-request-serializer.rakutest` boundary
    /// pattern that needs the INTERNAL backreference to keep working, ruling
    /// out plain capture erasure).
    CaptureIsolatedGroup(RegexPattern),
    /// [`RegexAtom::CaptureIsolatedGroup`], but the interpolated value was
    /// itself a closure (its pattern embeds `@(...)`/`$(...)`/`{...}` code —
    /// [`Value::RegexCaptured`]): `scope` is the lexical scope that code
    /// closed over, snapshotted at the point the *inner* regex literal was
    /// evaluated. A `<$re>` reference resolves `$re`'s value at the OUTER
    /// pattern's parse time and splices its pattern text in — so without
    /// this, the embedded code would resolve its free variables against
    /// whatever happens to be live at the outer match site instead of the
    /// scope it actually closed over (issue #8951). The match-time execution
    /// sites install `scope` into `env` for the duration of this atom's
    /// match, exactly like
    /// [`crate::runtime::Interpreter::install_regex_closure_scope`] does for
    /// a `RegexCaptured` matched directly.
    CaptureIsolatedGroupScoped(RegexPattern, Arc<crate::value::ValueMap>),
    Alternation(Vec<RegexPattern>),
    SequentialAlternation(Vec<RegexPattern>),
    /// Conjunction: all branches must match at the same position; longest match wins
    Conjunction(Vec<RegexPattern>),
    ZeroWidth,
    CodeAssertion {
        code: String,
        negated: bool,
        is_assertion: bool,
        /// Parser-produced code bodies can bypass the string reparse while
        /// retaining the same inline execution path as legacy regex values.
        body: Option<std::sync::Arc<Vec<crate::ast::Stmt>>>,
        /// Stable carrier-compile cache key for a parser-produced body.
        code_cache_id: u64,
    },
    /// `<{ code }>` — closure interpolation: evaluate code and match result as regex
    ClosureInterpolation {
        code: String,
        /// Parser-produced bodies avoid reparsing the source string while
        /// retaining the existing scratch-interpreter execution model.
        body: Option<std::sync::Arc<Vec<crate::ast::Stmt>>>,
    },
    UnicodeProp {
        name: String,
        negated: bool,
        args: Option<String>,
    },
    UnicodePropAssert {
        name: String,
        negated: bool,
    }, // zero-width assertion
    CaptureStartMarker,
    CaptureEndMarker,
    /// `:my $var = expr;` — variable declaration inside a regex
    VarDecl {
        code: String,
    },
    /// Combined character class: <+ xdigit - lower>, matches positive AND NOT negative
    CompositeClass {
        positive: Vec<ClassItem>,
        negative: Vec<ClassItem>,
    },
    /// Lookaround assertion: <?before pattern>, <!before pattern>,
    /// <?after pattern>, <!after pattern>
    Lookaround {
        pattern: RegexPattern,
        negated: bool,
        is_behind: bool,
    },
    /// `<<` or `«` — left word boundary assertion (zero-width)
    LeftWordBoundary,
    /// `>>` or `»` — right word boundary assertion (zero-width)
    RightWordBoundary,
    /// `<?wb>` (word boundary) / `<!wb>` (not a word boundary) — zero-width
    /// assertion at a transition between a word char and a non-word char (either
    /// direction), i.e. `<<` or `>>`.
    WordBoundary {
        negated: bool,
    },
    /// `<?ww>` (within word) / `<!ww>` (not within word) — zero-width assertion
    /// that the position sits *between two word characters*. Unlike
    /// [`RegexAtom::WordBoundary`] this is not the negation of a boundary: both
    /// are false in the middle of a run of non-word characters, because a
    /// position outside the string counts as non-word for either test.
    WithinWord {
        negated: bool,
    },
    /// `^^` — start of line assertion (zero-width)
    StartOfLine,
    /// `$$` — end of line assertion (zero-width)
    EndOfLine,
    /// A mid-pattern `$` — end of *string* assertion (zero-width). Raku's `$` is
    /// always end-of-string; only `$$` is end-of-line. A trailing `$` sets the
    /// pattern's `anchor_end` instead; this atom covers a `$` that is followed by
    /// something else (`^ .* $ { make … }`).
    EndOfString,
    /// `$0`, `$1`, etc. — backreference to positional capture group
    Backref(usize),
    /// `$<name>` — backreference to named capture group
    NamedBackref(String),
    /// Bare `$name` interpolating an in-regex `:my $name …` lexical: a match-time
    /// interpolation of the variable's string value as a literal. The value is
    /// read from `caps.regex_vars` (falling back to `env`) when the atom is
    /// matched, so it reflects assignments made earlier in the same match (e.g.
    /// a captured indentation string). Distinct from `NamedBackref` (which reads
    /// a capture) and from pre-substituted outer-scope `$var` interpolation.
    VarInterp(String),
    /// A `$( code )` / `@( code )` contextualizer interpolation — or a
    /// `"…$x.meth()…"` method-call chain inside a double-quoted atom, which
    /// the interpolation pre-pass rewrites to `$( $x.meth() )` — evaluated
    /// when the atom is matched, on the running interpreter (#10157). The
    /// scalar form (`list: false`) matches the result's string value
    /// literally; the list form matches an alternation over the elements
    /// (a `Regex` element as a sub-regex, anything else literally). Opaque
    /// to every static analysis, like [`RegexAtom::VarInterp`]: Rakudo
    /// compiles the atom to code, which ends a declarative prefix (ADR-0046
    /// probe Q).
    CodeInterp {
        code: Box<str>,
        list: bool,
    },
    /// A double-quoted atom (`"x @a[0]"`) the compiler lowered to a qq
    /// thunk (`crate::regex_qq_atoms`) whose result was not installed when
    /// the pattern was parsed: a `<$re>`-interpolated regex (its scope is
    /// installed only when the atom is matched, by
    /// [`RegexAtom::CaptureIsolatedGroupScoped`]) or a rule body parsed
    /// outside its resolve-and-match window (`regex_qq_token_scope`). At
    /// match time the thunk's string result is read from `env` under `key`
    /// (a `MetaNs::RegexQq` key) and matched literally; with no result there
    /// (the atom was never lowered, or its thunk threw) `fallback`, the
    /// atom's text-scan reading, is matched instead. Opaque to every static
    /// analysis, like [`RegexAtom::VarInterp`] — Rakudo compiles the atom to
    /// code, which ends a declarative prefix.
    QqInterp {
        key: crate::symbol::Symbol,
        fallback: Box<RegexPattern>,
    },
    /// `<?same>` / `<!same>` — zero-width assertion: adjacent chars are same/different
    SameAssertion {
        negated: bool,
    },
    /// `<at(N)>` — zero-width assertion: match at position N
    AtPosition(usize),
    /// `<~~>` — recursive self-match: match the *enclosing* regex (or, inside a
    /// grammar token/rule, that rule's body) again at the current position. The
    /// payload is the enclosing pattern's source text, which the match-time
    /// parse cache turns straight back into the same shared tree. Like the
    /// `<$var>` sub-match family, the recursive invocation gets its own
    /// discarded `Match`, so its captures do not leak into the caller's `$/`.
    RecurseSelf(Box<str>),
    /// Internal marker used while rewriting `left ~ goal inner`.
    TildeMarker,
    /// Goal matching produced by `~`: match `inner` first, then `goal`,
    /// but preserve capture order as written (`goal` before `inner`).
    GoalMatch {
        goal: RegexPattern,
        inner: RegexPattern,
        goal_text: String,
    },
}

#[derive(Clone)]
pub(crate) enum RegexQuant {
    One,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    /// `** min..max` — repeat exactly min to max times (max=None means unbounded)
    Repeat(usize, Option<usize>),
    /// `** {code}` — repeat count determined at runtime by evaluating code block
    RepeatCode(String),
}

#[derive(Clone)]
pub(crate) struct CharClass {
    pub(crate) negated: bool,
    pub(crate) items: Vec<ClassItem>,
}

#[derive(Clone)]
pub(crate) enum ClassItem {
    Range(char, char),
    Char(char),
    /// A class entry that is one grapheme but several codepoints, such as
    /// `<[क्ष]>` or the `\c[LATIN CAPITAL LETTER A WITH HOOK ABOVE,HEBREW POINT
    /// HIRIQ]` spelling of the same thing.
    ///
    /// A class matches a whole grapheme, so such an entry has to survive as one
    /// item: stored as its separate codepoints it could only match by starting
    /// inside the cluster, which is not a position any atom may start at. NFC
    /// collapses many base-plus-mark sequences to a single `char`, and those
    /// stay [`ClassItem::Char`]; this variant is for the ones it does not.
    ///
    /// It is a legal class *entry* but not a legal range *endpoint* — `a..क्ष`
    /// is still rejected at parse time.
    Grapheme(Box<str>),
    /// The `.` any-character base of a character-class arithmetic expression
    /// (`<.-[a]-[b]>`, `<.-:letter-:digit>`): a positive item that matches every
    /// character. Raku's class arithmetic is not true set arithmetic — the
    /// positive and negative halves are accumulated separately and a character
    /// matches when it is in the positive half AND NOT in the negative half —
    /// so the universe has to survive as an *item*, not by collapsing the whole
    /// positive half (`<.-[a]+[1]>` still excludes `a`).
    Any,
    Digit,
    NegDigit,
    Word,
    NegWord,
    Space,
    NegSpace,
    HorizSpace,
    NegHorizSpace,
    VertSpace,
    NegVertSpace,
    NotNewline,
    NamedBuiltin(String),
    UnicodePropItem {
        name: String,
        negated: bool,
    },
}
