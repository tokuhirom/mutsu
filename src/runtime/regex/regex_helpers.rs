use super::super::*;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use unicode_normalization::UnicodeNormalization;
use unicode_normalization::char::is_combining_mark;
use unicode_segmentation::UnicodeSegmentation;

thread_local! {
    pub(super) static PENDING_REGEX_GOAL_FAILURE: RefCell<Option<(String, usize)>> = const { RefCell::new(None) };
    /// Collects plain (non-assertion) code blocks that should be executed eagerly
    /// during regex matching, even if the overall match fails. Used by `comb` etc.
    pub(crate) static EAGER_CODE_BLOCKS: RefCell<Option<Vec<CodeBlockContext>>> = const { RefCell::new(None) };
    /// The character immediately preceding the start of the text currently being
    /// matched. A subrule (`<foo>`) is matched against a *slice* `chars[pos..]`
    /// in a fresh sub-interpreter, so position 0 of that slice is not necessarily
    /// the start of a line in the original text. Look-behind anchors (`^^`, `^`)
    /// consult this to decide whether slice-position 0 is a real line/string
    /// start: `None` means it truly is the start of the parse text, `Some(c)`
    /// means `c` was the char just before the slice. Saved/restored around each
    /// subrule dispatch so nesting threads correctly.
    pub(crate) static REGEX_PRECEDING_CHAR: Cell<Option<char>> = const { Cell::new(None) };
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
            *b = Some(HashMap::new());
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
            REGEX_DYNVAR_OVERLAY.with(|slot| slot.borrow_mut().replace(HashMap::new()));
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

/// Restores `REGEX_PRECEDING_CHAR` to a saved value when dropped, so a subrule
/// dispatch can publish the slice's preceding char for the duration of the match
/// and have it restored on every exit path (including early returns).
pub(crate) struct RegexPrecedingCharGuard(pub(crate) Option<char>);

impl Drop for RegexPrecedingCharGuard {
    fn drop(&mut self) {
        REGEX_PRECEDING_CHAR.with(|c| c.set(self.0));
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
pub(super) fn strip_marks_text(orig_chars: &[char]) -> (Vec<char>, Vec<usize>) {
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

/// Strip combining marks from all literal atoms in a RegexPattern (recursively).
pub(super) fn strip_marks_pattern(pattern: &RegexPattern) -> RegexPattern {
    RegexPattern {
        tokens: pattern.tokens.iter().map(strip_marks_token).collect(),
        anchor_start: pattern.anchor_start,
        anchor_end: pattern.anchor_end,
        ignore_case: pattern.ignore_case,
        ignore_mark: false,
    }
}

fn strip_marks_token(token: &RegexToken) -> RegexToken {
    RegexToken {
        atom: strip_marks_atom(&token.atom),
        quant: token.quant.clone(),
        named_capture: token.named_capture.clone(),
        secondary_named_capture: token.secondary_named_capture.clone(),
        hash_capture: token.hash_capture.clone(),
        ratchet: token.ratchet,
        frugal: token.frugal,
        separator: token.separator.as_ref().map(|s| {
            Box::new(RegexSeparatorSpec {
                pattern: strip_marks_pattern(&s.pattern),
                allow_trailing: s.allow_trailing,
            })
        }),
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
        RegexAtom::Named(name) => {
            // Named subrule / literal string match — strip marks from the name
            let stripped: String = name.nfd().filter(|c| !is_combining_mark(*c)).collect();
            RegexAtom::Named(stripped)
        }
        RegexAtom::Group(p) => RegexAtom::Group(strip_marks_pattern(p)),
        RegexAtom::CaptureGroup(p) => RegexAtom::CaptureGroup(strip_marks_pattern(p)),
        RegexAtom::Alternation(alts) => {
            RegexAtom::Alternation(alts.iter().map(strip_marks_pattern).collect())
        }
        RegexAtom::SequentialAlternation(alts) => {
            RegexAtom::SequentialAlternation(alts.iter().map(strip_marks_pattern).collect())
        }
        RegexAtom::Conjunction(branches) => {
            RegexAtom::Conjunction(branches.iter().map(strip_marks_pattern).collect())
        }
        RegexAtom::GoalMatch {
            goal,
            inner,
            goal_text,
        } => RegexAtom::GoalMatch {
            goal: strip_marks_pattern(goal),
            inner: strip_marks_pattern(inner),
            goal_text: goal_text.clone(),
        },
        RegexAtom::Lookaround {
            pattern,
            negated,
            is_behind,
        } => RegexAtom::Lookaround {
            pattern: strip_marks_pattern(pattern),
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

/// Map a position from stripped char space back to original char space.
pub(super) fn map_pos(pos: usize, pos_map: &[usize], orig_len: usize) -> usize {
    if pos < pos_map.len() {
        pos_map[pos]
    } else {
        orig_len
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

pub(super) struct NamedRegexLookupSpec {
    pub(super) silent: bool,
    pub(super) token_lookup: bool,
    pub(super) lookup_name: String,
    pub(super) capture_name: Option<String>,
    pub(super) arg_exprs: Vec<String>,
    /// When true, the alias replaces the original capture name (dot-call alias).
    /// `<foo=.alpha>` sets this to true; `<foo=alpha>` leaves it false.
    pub(super) alias_replaces_original: bool,
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
pub(super) fn grapheme_end(chars: &[char], pos: usize) -> usize {
    // \r\n is a single grapheme cluster in Raku
    if pos < chars.len() && chars[pos] == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
        return pos + 2;
    }
    let mut end = pos + 1;
    while end < chars.len() && is_combining_mark(chars[end]) {
        end += 1;
    }
    end
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
        dst.named.entry(k).or_default().extend(v);
    }
    for (k, v) in src.named_subcaps.drain() {
        dst.named_subcaps.entry(k).or_default().extend(v);
    }
    dst.named_quantified.extend(src.named_quantified.drain());
    for (k, v) in src.capture_alias_map.drain() {
        dst.capture_alias_map.insert(k, v);
    }
    dst.positional.append(&mut src.positional);
    dst.positional_subcaps.append(&mut src.positional_subcaps);
    dst.positional_quantified
        .append(&mut src.positional_quantified);
    dst.positional_offsets.append(&mut src.positional_offsets);
    dst.code_blocks.append(&mut src.code_blocks);
    for (k, v) in src.hash_captures.drain() {
        dst.hash_captures.entry(k).or_default().extend(v);
    }
    dst
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
    let mut count = 0;
    for token in &pat.tokens {
        count += count_capture_groups(&token.atom);
    }
    count
}

/// Fold quantified captures. After a quantifier loop, positional entries from
/// `base_len` onward may contain repeated captures from multiple iterations.
/// If `stride` > 0 (captures per iteration), fold them into quantified lists.
pub(super) fn fold_quantified_captures(caps: &mut RegexCaptures, base_len: usize, stride: usize) {
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
            caps.positional.push(String::new());
            caps.positional_subcaps.push(None);
            caps.positional_quantified.push(Some(Vec::new()));
            caps.positional_offsets.push((0, 0));
        }
        return;
    }
    if new_entries <= stride {
        // Exactly one iteration — mutsu keeps the single capture un-folded.
        // TODO: Raku makes `*`/`+` always a List even for one match (`(z)*`
        // matching once yields `List(1)`); folding here has wider blast radius.
        return;
    }
    let iterations = new_entries / stride;
    if iterations * stride != new_entries {
        // Uneven — don't fold (shouldn't happen with well-formed captures)
        return;
    }

    // Collect entries per group
    let mut folded_positional = Vec::with_capacity(stride);
    let mut folded_subcaps = Vec::with_capacity(stride);
    let mut folded_quantified: Vec<Option<Vec<QuantifiedCaptureEntry>>> =
        Vec::with_capacity(stride);

    for group in 0..stride {
        let mut list: Vec<QuantifiedCaptureEntry> = Vec::with_capacity(iterations);
        for iter in 0..iterations {
            let idx = base_len + iter * stride + group;
            let text = caps.positional[idx].clone();
            let subcap = if idx < caps.positional_subcaps.len() {
                caps.positional_subcaps[idx].clone()
            } else {
                None
            };
            let (from, to) = if idx < caps.positional_offsets.len() {
                caps.positional_offsets[idx]
            } else {
                (0, text.chars().count())
            };
            list.push((text, from, to, subcap));
        }
        // Use the last iteration's values as the "representative" for backref purposes
        let last = list.last().unwrap();
        folded_positional.push(last.0.clone());
        folded_subcaps.push(last.3.clone());
        folded_quantified.push(Some(list));
    }

    // Replace entries from base_len onward
    caps.positional.truncate(base_len);
    caps.positional.extend(folded_positional);
    caps.positional_subcaps.truncate(base_len);
    caps.positional_subcaps.extend(folded_subcaps);
    // Ensure positional_quantified is the right length
    while caps.positional_quantified.len() < base_len {
        caps.positional_quantified.push(None);
    }
    caps.positional_quantified.truncate(base_len);
    caps.positional_quantified.extend(folded_quantified);
    // Also truncate offsets if present
    if caps.positional_offsets.len() > base_len {
        caps.positional_offsets.truncate(base_len);
    }
}

/// Reserve `stride` index-stable Nil slots for an unmatched optional capture
/// group (`(x)?` that matched zero times). The slots render as `Nil` in the
/// resulting Match (Raku: `(a)?(b)` on "b" yields `$0 = Nil`, `$1 = b`).
///
/// `positional_nil` is padded with `false` up to the current `positional` length
/// before the `true` entries are pushed, so the matched captures that precede
/// this group (which never touch `positional_nil`) stay index-aligned.
pub(super) fn reserve_nil_capture_slots(caps: &mut RegexCaptures, stride: usize) {
    if stride == 0 {
        return;
    }
    while caps.positional_nil.len() < caps.positional.len() {
        caps.positional_nil.push(false);
    }
    let at = caps.to;
    for _ in 0..stride {
        caps.positional.push(String::new());
        caps.positional_subcaps.push(None);
        caps.positional_quantified.push(None);
        caps.positional_offsets.push((at, at));
        caps.positional_nil.push(true);
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
        "alnum" => c.is_alphabetic() || c == '_' || c.is_ascii_digit(),
        "blank" => c == '\t' || c == ' ' || c == '\u{A0}',
        "cntrl" => c.is_control(),
        "punct" => check_unicode_property("Punctuation", c),
        "graph" => {
            // graph: visible characters — not whitespace, not control, not unassigned surrogates
            !c.is_whitespace() && !c.is_control()
        }
        "print" => {
            // print: graph + space-like characters (but not control characters)
            !c.is_control()
        }
        _ => false,
    }
}
