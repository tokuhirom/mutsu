//! Data types for the regex engine — the parsed-pattern representation
//! (`RegexPattern`/`RegexToken`/`RegexAtom`/…), the capture accumulator
//! (`RegexCaptures`), and the character-class model (`CharClass`/`ClassItem`).
//!
//! Extracted verbatim from `runtime/mod.rs` (2026-07-21 hygiene re-slim). These
//! were parent-module-private structs whose fields the sibling `regex`/
//! `regex_parse*` modules read directly; moving them into their own module
//! widens the previously module-private structs and fields to `pub(crate)` so
//! those siblings keep their access (the whole set is re-exported from
//! `runtime` via `pub(crate) use self::regex_types::*`).

use crate::value::Value;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

#[derive(Clone)]
pub(crate) struct RegexPattern {
    pub(crate) tokens: Vec<RegexToken>,
    pub(crate) anchor_start: bool,
    pub(crate) anchor_end: bool,
    pub(crate) ignore_case: bool,
    pub(crate) ignore_mark: bool,
}

/// Context stored for each code block encountered during regex matching.
#[derive(Clone)]
pub(crate) struct CodeBlockContext {
    pub(crate) code: String,
    pub(crate) named: HashMap<String, Vec<String>>,
    pub(crate) matched_so_far: String,
    pub(crate) positional: Vec<String>,
}

/// A single entry in a quantified capture list: (matched_text, from, to, subcaptures).
///
/// The nested sub-captures are held behind an `Arc` so that cloning a parent
/// `RegexCaptures` during backtracking is a refcount bump rather than a deep
/// copy of the whole sub-match tree. A completed sub-match is effectively
/// immutable once stored; the rare post-store tweak (e.g. setting `action_name`)
/// goes through `Arc::make_mut`, which is free while the entry is still unshared.
pub(crate) type QuantifiedCaptureEntry = (String, usize, usize, Option<Arc<RegexCaptures>>);

/// Prefix marking a `named_subcaps` entry as a *silent action capture*: the
/// match of a silent subrule (`<.foo>`) that is hidden from `.hash` but whose
/// grammar action method (and its descendants') must still fire. The prefix is a
/// control character that can never appear in a real capture name, so marker
/// entries never collide with user captures and are trivially filtered.
pub(crate) const SILENT_ACTION_MARKER_PREFIX: &str = "\u{1}silent\u{1}";

#[derive(Clone, Default)]
pub(crate) struct RegexCaptures {
    pub(crate) named: HashMap<String, Vec<String>>,
    /// Nested sub-captures for named subrule matches. Key is capture name,
    /// value is inner captures from the subrule (parallel to entries in `named`).
    pub(crate) named_subcaps: HashMap<String, Vec<Arc<RegexCaptures>>>,
    pub(crate) positional: Vec<String>,
    /// Nested sub-captures for positional capture groups. Each entry corresponds
    /// to the same index in `positional` and holds inner captures from nested groups.
    pub(crate) positional_subcaps: Vec<Option<Arc<RegexCaptures>>>,
    /// When a capture group is quantified (e.g. `(\w)+`), this parallel vec
    /// stores the list of all iteration matches for that positional index.
    /// When `Some`, the positional slot should be rendered as an Array of Match objects.
    pub(crate) positional_quantified: Vec<Option<Vec<QuantifiedCaptureEntry>>>,
    /// Character offsets (start, end) for each entry in `positional`.
    pub(crate) positional_offsets: Vec<(usize, usize)>,
    /// Marks a positional slot as an *unmatched optional* capture (`(x)?` that
    /// matched zero times) which must render as `Nil` (not an empty Match). Only
    /// the zero-match reservation arms set entries here; they pad with `false` up
    /// to the current `positional` length before pushing `true`, so matched
    /// captures (which never touch this vec) stay aligned. The Match builder reads
    /// it via `.get(i)` — a missing/`false` entry renders normally.
    pub(crate) positional_nil: Vec<bool>,
    /// Unnamed capture slots by capture index (for $0, $1, ...), where `None`
    /// represents an unmatched capture.
    pub(crate) positional_slots: Vec<Option<(String, usize, usize)>>,
    pub(crate) matched: String,
    pub(crate) from: usize,
    pub(crate) to: usize,
    pub(crate) capture_start: Option<usize>,
    pub(crate) capture_end: Option<usize>,
    /// Starting position of the match in the input (character index).
    /// Set at the beginning of regex matching to allow code blocks to compute
    /// the matched-so-far text.
    pub(crate) match_from: usize,
    /// Code blocks encountered during matching (code + captures at that point).
    /// Executed after match for side effects.
    /// Each entry: (code, named_captures, matched_so_far, positional_captures)
    pub(crate) code_blocks: Vec<CodeBlockContext>,
    /// Variables declared via `:my $var = expr;` inside regex.
    /// These are made available to `<{ code }>` closures.
    pub(crate) regex_vars: HashMap<String, Value>,
    /// The winning :sym<> variant name, if this match was from a protoregex.
    pub(crate) sym: Option<String>,
    /// Named captures from quantified tokens — always stored as arrays in Match.
    pub(crate) named_quantified: HashSet<String>,
    /// For aliased captures like `<str=.str_escape>`, maps capture name to
    /// original rule name for grammar action dispatch.
    pub(crate) capture_alias_map: HashMap<String, String>,
    /// The original rule name when this capture was stored under an alias.
    pub(crate) action_name: Option<String>,
    /// Hash captures from `%<name>=(...)` aliasing in regex.
    pub(crate) hash_captures: HashMap<String, Vec<(String, Option<String>)>>,
    /// The AST value produced by this node's inline `{ make … }` code block(s),
    /// computed at reduce time (`reduce_regex_captures_made`). Carried into the
    /// Match object built by `make_match_object_full_q` so `$<sub>.made` /
    /// `$<sub>».made` resolve in a parent inline action and post-parse. `None`
    /// when the rule ran no `make`.
    pub(crate) ast: Option<Value>,
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
}

#[derive(Clone)]
pub(crate) struct RegexSeparatorSpec {
    /// The separator sub-pattern (matched between iterations). Holding a full
    /// pattern preserves named captures, quantifiers, and other structure of
    /// complex separators such as `$<delim>=<[a..z]>*`.
    pub(crate) pattern: RegexPattern,
    pub(crate) allow_trailing: bool,
}

#[derive(Clone)]
pub(crate) enum RegexAtom {
    Literal(char),
    Named(String),
    Any,
    CharClass(CharClass),
    /// `<.ws>` — Raku's word-boundary-aware whitespace rule:
    /// requires `\s+` between word characters, `\s*` otherwise.
    WsRule,
    Newline,
    NotNewline,
    Group(RegexPattern),
    CaptureGroup(RegexPattern),
    Alternation(Vec<RegexPattern>),
    SequentialAlternation(Vec<RegexPattern>),
    /// Conjunction: all branches must match at the same position; longest match wins
    Conjunction(Vec<RegexPattern>),
    ZeroWidth,
    CodeAssertion {
        code: String,
        negated: bool,
        is_assertion: bool,
    },
    /// `<{ code }>` — closure interpolation: evaluate code and match result as regex
    ClosureInterpolation {
        code: String,
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
    /// `^^` — start of line assertion (zero-width)
    StartOfLine,
    /// `$$` — end of line assertion (zero-width)
    EndOfLine,
    /// `$0`, `$1`, etc. — backreference to positional capture group
    Backref(usize),
    /// `$<name>` — backreference to named capture group
    NamedBackref(String),
    /// `<?same>` / `<!same>` — zero-width assertion: adjacent chars are same/different
    SameAssertion {
        negated: bool,
    },
    /// `<at(N)>` — zero-width assertion: match at position N
    AtPosition(usize),
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
    UnicodePropItem { name: String, negated: bool },
}
