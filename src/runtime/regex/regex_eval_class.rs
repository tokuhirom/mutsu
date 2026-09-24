use super::super::*;
use super::regex_helpers::{CaseFoldIter, matches_named_builtin};
use super::regex_prefilter::regex_scan_positions;
use crate::builtins::cclass;

impl Interpreter {
    pub(super) fn regex_match_class_ignorecase(
        &mut self,
        class: &CharClass,
        c: char,
        ignore_case: bool,
    ) -> bool {
        class_matches_ignorecase(class, c, ignore_case)
    }
}

/// Whether `c` is in `class`, with `:i` applied when `ignore_case`.
///
/// A free function, not an `Interpreter` method, because the whole
/// class evaluator is pure — and the ADR-0099 Stage 1 scan prefilter
/// (`regex_prefilter_analysis.rs`) needs to evaluate a class over the ASCII
/// range at *analysis* time, where there is no `&mut Interpreter` to hand and
/// where a second, independently-written membership table would be exactly the
/// silent-drift hazard the ADR's constraint 1 warns about. Deriving the
/// prefilter's first-set through the engine's own predicate makes the two
/// unable to disagree.
pub(super) fn class_matches_ignorecase(class: &CharClass, c: char, ignore_case: bool) -> bool {
    if !ignore_case {
        return class_matches(class, c);
    }
    // `:i` folds a literal character or an explicit range, but not membership
    // of a named built-in class or a Unicode property: `<upper>` still means
    // "an uppercase character" under `:i`, not "any case variant of one" —
    // rakudo's `:i` governs literal comparison, not class membership (#8498).
    // Handled per item (rather than folding the whole class at once, as
    // before) so a class mixing a literal with a named builtin still folds
    // the literal half.
    let matched = class.items.iter().any(|item| match item {
        ClassItem::NamedBuiltin(name) => matches_named_builtin(name, c),
        ClassItem::UnicodePropItem { name, negated } => {
            let m = check_unicode_property(name, c);
            if *negated { !m } else { m }
        }
        _ => {
            let single_item_class = CharClass {
                items: vec![item.clone()],
                negated: false,
            };
            CaseFoldIter::new(c).any(|variant| class_matches(&single_item_class, variant))
        }
    });
    if class.negated { !matched } else { matched }
}

/// Whether `c` is in `class` (case-sensitively). See
/// [`class_matches_ignorecase`] for why this is a free function.
pub(super) fn class_matches(class: &CharClass, c: char) -> bool {
    {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                // A multi-codepoint grapheme can never equal one `char`. The
                // whole-grapheme comparison happens at the atom, which is the
                // only place that has the subject text to compare against; see
                // the `CharClass` arm in `regex_match_atom_simple`.
                ClassItem::Grapheme(_) => {}
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if cclass::is_digit(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegDigit => {
                    if !cclass::is_digit(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if cclass::is_word(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegWord => {
                    if !cclass::is_word(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if cclass::is_space(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegSpace => {
                    if !cclass::is_space(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::HorizSpace => {
                    if matches!(
                        c,
                        ' ' | '\t' | '\u{00A0}' | '\u{1680}' | '\u{2000}'
                            ..='\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}'
                    ) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegHorizSpace => {
                    if !matches!(
                        c,
                        ' ' | '\t' | '\u{00A0}' | '\u{1680}' | '\u{2000}'
                            ..='\u{200A}' | '\u{202F}' | '\u{205F}' | '\u{3000}'
                    ) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::VertSpace => {
                    if matches!(
                        c,
                        '\n' | '\r'
                            | '\u{000B}'
                            | '\u{000C}'
                            | '\u{0085}'
                            | '\u{2028}'
                            | '\u{2029}'
                    ) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegVertSpace => {
                    if !matches!(
                        c,
                        '\n' | '\r'
                            | '\u{000B}'
                            | '\u{000C}'
                            | '\u{0085}'
                            | '\u{2028}'
                            | '\u{2029}'
                    ) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NotNewline => {
                    if !cclass::is_newline(c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Any => {
                    matched = true;
                    break;
                }
                ClassItem::NamedBuiltin(name) => {
                    if matches_named_builtin(name, c) {
                        matched = true;
                        break;
                    }
                }
                ClassItem::UnicodePropItem { name, negated } => {
                    let prop_match = check_unicode_property(name, c);
                    if if *negated { !prop_match } else { prop_match } {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }
}

impl Interpreter {
    /// Find all non-overlapping regex matches using the capturing path,
    /// returning (start, end) pairs and captures (including code blocks).
    /// The search (and any code block's side effects) stops after `limit`
    /// matches.
    pub(crate) fn regex_find_all_with_caps_limited(
        &mut self,
        pattern: &str,
        text: &str,
        limit: usize,
    ) -> Vec<(usize, usize, RegexCaptures)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package_sym();
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let chars = target.chars();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() && results.len() < limit {
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some((end, mut caps)) =
                        self.regex_match_end_from_caps_in_pkg(&parsed, chars, 0, pkg)
                {
                    caps.from = caps.capture_start.unwrap_or(0);
                    caps.to = caps.capture_end.unwrap_or(end);
                    caps.set_target(Some(target.clone()));
                    found = Some((0, end, caps));
                }
            } else {
                for start in regex_scan_positions(self, &parsed, chars, pos, pkg) {
                    if let Some((end, mut caps)) =
                        self.regex_match_end_from_caps_in_pkg(&parsed, chars, start, pkg)
                    {
                        caps.from = caps.capture_start.unwrap_or(start);
                        caps.to = caps.capture_end.unwrap_or(end);
                        caps.set_target(Some(target.clone()));
                        found = Some((start, end, caps));
                        break;
                    }
                }
            }
            match found {
                Some((start, end, caps)) => {
                    results.push((start, end, caps));
                    pos = if end > start { end } else { start + 1 };
                }
                None => break,
            }
            if parsed.anchor_start {
                break;
            }
        }
        results
    }

    /// Find all non-overlapping regex matches, returning (start, end) char-index pairs.
    pub(crate) fn regex_find_all(&mut self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        self.regex_find_all_limited(pattern, text, usize::MAX)
    }

    /// [`Interpreter::regex_find_all`] stopping after `limit` matches.
    pub(crate) fn regex_find_all_limited(
        &mut self,
        pattern: &str,
        text: &str,
        limit: usize,
    ) -> Vec<(usize, usize)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package_sym();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() && results.len() < limit {
            let search_start = if parsed.anchor_start { 0 } else { pos };
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, 0, pkg)
                {
                    found = Some((0, end));
                }
            } else {
                for start in regex_scan_positions(self, &parsed, &chars, search_start, pkg) {
                    if let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, start, pkg)
                    {
                        found = Some((start, end));
                        break;
                    }
                }
            }
            match found {
                Some((start, end)) => {
                    results.push((start, end));
                    // Advance past the match (at least 1 to avoid infinite loop)
                    pos = if end > start { end } else { start + 1 };
                }
                None => break,
            }
            if parsed.anchor_start {
                break;
            }
        }
        results
    }
}

/// The characters the `<+a -b>` composite-class arm tests one class item
/// against, at a subject character it has already resolved to `effective_c`
/// (`\r` of a `\r\n` cluster arrives here as `\n`).
///
/// A free function for the same reason [`class_matches_ignorecase`] is one:
/// the ADR-0099 Stage 1 scan prefilter has to ask the same question at
/// *analysis* time, over the ASCII range, and a second reading of what `:i`
/// expands a character to would be exactly the silent-drift hazard the ADR's
/// constraint 1 warns about.
pub(super) fn composite_probe_chars(effective_c: char, ignore_case: bool) -> Vec<char> {
    if ignore_case {
        CaseFoldIter::new(effective_c).collect()
    } else {
        vec![effective_c]
    }
}

/// Whether a composite-class item matches on **character evidence alone** —
/// the built-in predicate, the Unicode property, or the plain class item.
///
/// This is the whole of the arm's test except for one thing: a
/// [`ClassItem::NamedBuiltin`] the built-in predicate rejects falls back, in
/// the engine, to resolving a *grammar token* of that name and matching it
/// against the remaining input. That half needs the subject and the invocant
/// package, so it stays at the call site (`regex_match_atom_simple.rs`) and
/// the prefilter handles it by declining (`regex_prefilter_composite.rs`).
///
/// Answering `true` here therefore means the engine's arm answers `true` too,
/// in either direction: the character half runs first and short-circuits. That
/// is what lets the prefilter both *admit* a character on this evidence and
/// *reject* one a negative item matches on it.
pub(super) fn composite_item_matches(item: &ClassItem, chars_to_check: &[char]) -> bool {
    // `chars_to_check[0]` is always the subject's own (unfolded) character:
    // `composite_probe_chars` -- via `CaseFoldIter::new` -- pushes it first
    // before any case-fold variant, and returns a single-element slice
    // holding just it when `:i` is off.
    let Some(&orig_c) = chars_to_check.first() else {
        return false;
    };
    match item {
        // `:i` folds a literal character/range comparison, but not membership
        // of a named built-in class or a Unicode property: `<+upper>` still
        // means "an uppercase character" under `:i`, not "any case variant of
        // one" -- test the subject's own character only (#8498).
        ClassItem::NamedBuiltin(n) => matches_named_builtin(n, orig_c),
        ClassItem::UnicodePropItem { name, negated } => {
            let m = check_unicode_property(name, orig_c);
            if *negated { !m } else { m }
        }
        _ => {
            let class = CharClass {
                items: vec![item.clone()],
                negated: false,
            };
            chars_to_check.iter().any(|ch| class_matches(&class, *ch))
        }
    }
}
