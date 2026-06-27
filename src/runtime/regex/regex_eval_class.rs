use super::super::*;
use super::regex_helpers::{CaseFoldIter, matches_named_builtin};

impl Interpreter {
    pub(super) fn regex_match_class_ignorecase(
        &self,
        class: &CharClass,
        c: char,
        ignore_case: bool,
    ) -> bool {
        if ignore_case {
            if class.negated {
                // For negated classes with :i, char matches only if ALL case
                // variants are NOT in the positive set. If any variant IS in the
                // positive set the char should be excluded by the negation.
                let pos_class = CharClass {
                    negated: false,
                    items: class.items.clone(),
                };
                for variant in CaseFoldIter::new(c) {
                    if self.regex_match_class(&pos_class, variant) {
                        return false;
                    }
                }
                return true;
            }
            // For positive classes, any case variant matching is sufficient
            for variant in CaseFoldIter::new(c) {
                if self.regex_match_class(class, variant) {
                    return true;
                }
            }
            return false;
        }
        self.regex_match_class(class, c)
    }

    pub(super) fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegDigit => {
                    if !c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegWord => {
                    if !(c.is_alphanumeric() || c == '_') {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::NegSpace => {
                    if !c.is_whitespace() {
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
                    if c != '\n' && c != '\r' {
                        matched = true;
                        break;
                    }
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

    /// Find all non-overlapping regex matches using the capturing path,
    /// returning (start, end) pairs and captures (including code blocks).
    pub(crate) fn regex_find_all_with_caps(
        &self,
        pattern: &str,
        text: &str,
    ) -> Vec<(usize, usize, RegexCaptures)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() {
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some((end, mut caps)) =
                        self.regex_match_end_from_caps_in_pkg(&parsed, &chars, 0, &pkg)
                {
                    caps.from = caps.capture_start.unwrap_or(0);
                    caps.to = caps.capture_end.unwrap_or(end);
                    caps.matched = chars[caps.from..caps.to].iter().collect();
                    found = Some((0, end, caps));
                }
            } else {
                for start in pos..=chars.len() {
                    if let Some((end, mut caps)) =
                        self.regex_match_end_from_caps_in_pkg(&parsed, &chars, start, &pkg)
                    {
                        caps.from = caps.capture_start.unwrap_or(start);
                        caps.to = caps.capture_end.unwrap_or(end);
                        caps.matched = chars[caps.from..caps.to].iter().collect();
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
    pub(crate) fn regex_find_all(&self, pattern: &str, text: &str) -> Vec<(usize, usize)> {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return Vec::new(),
        };
        let pkg = self.current_package();
        let chars: Vec<char> = text.chars().collect();
        let mut results = Vec::new();
        let mut pos = 0;
        while pos <= chars.len() {
            let search_start = if parsed.anchor_start { 0 } else { pos };
            let mut found = None;
            if parsed.anchor_start {
                if pos == 0
                    && let Some(end) = self.regex_match_end_from_in_pkg(&parsed, &chars, 0, &pkg)
                {
                    found = Some((0, end));
                }
            } else {
                for start in search_start..=chars.len() {
                    if let Some(end) =
                        self.regex_match_end_from_in_pkg(&parsed, &chars, start, &pkg)
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
