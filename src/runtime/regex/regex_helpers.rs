use super::super::*;
use std::cell::RefCell;
use unicode_normalization::UnicodeNormalization;
use unicode_normalization::char::is_combining_mark;
use unicode_segmentation::UnicodeSegmentation;

thread_local! {
    pub(super) static PENDING_REGEX_GOAL_FAILURE: RefCell<Option<(String, usize)>> = const { RefCell::new(None) };
    /// Collects plain (non-assertion) code blocks that should be executed eagerly
    /// during regex matching, even if the overall match fails. Used by `comb` etc.
    pub(crate) static EAGER_CODE_BLOCKS: RefCell<Option<Vec<CodeBlockContext>>> = const { RefCell::new(None) };
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
        // Among the bases, also drop Prepend characters (GCB=Prepend): these
        // are format characters (Cf) at specific codepoints that form part of
        // the grapheme cluster but are not the "base" letter.
        let filtered: Vec<char> = bases.into_iter().filter(|c| !is_prepend_char(*c)).collect();
        if filtered.is_empty() {
            // The entire grapheme is marks/prepends with no base — keep the
            // first non-combining char so the grapheme is not silently lost.
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

/// Strip combining marks from all literal atoms in a RegexPattern (recursively).
pub(super) fn strip_marks_pattern(pattern: &RegexPattern) -> RegexPattern {
    RegexPattern {
        tokens: pattern.tokens.iter().map(strip_marks_token).collect(),
        anchor_start: pattern.anchor_start,
        anchor_end: pattern.anchor_end,
        ignore_case: pattern.ignore_case,
        ignore_mark: pattern.ignore_mark,
    }
}

fn strip_marks_token(token: &RegexToken) -> RegexToken {
    RegexToken {
        atom: strip_marks_atom(&token.atom),
        quant: token.quant.clone(),
        named_capture: token.named_capture.clone(),
        ratchet: token.ratchet,
        frugal: token.frugal,
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
    dst.positional.append(&mut src.positional);
    dst.positional_subcaps.append(&mut src.positional_subcaps);
    dst.positional_quantified
        .append(&mut src.positional_quantified);
    dst.code_blocks.append(&mut src.code_blocks);
    dst
}

/// Count how many positional capture groups the given atom will produce.
pub(super) fn count_capture_groups(atom: &RegexAtom) -> usize {
    match atom {
        RegexAtom::CaptureGroup(_) => 1,
        RegexAtom::Group(pat) => count_pattern_capture_groups(pat),
        RegexAtom::Alternation(alts) => {
            // All alternatives should produce the same number of captures
            alts.iter()
                .map(count_pattern_capture_groups)
                .max()
                .unwrap_or(0)
        }
        _ => 0,
    }
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
    if new_entries <= stride {
        // Only one iteration or fewer — nothing to fold
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
        _ => false,
    }
}
