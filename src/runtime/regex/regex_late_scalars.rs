//! Plain `$var` atoms read at match time (ADR-12917, direction 2).
//!
//! ```raku
//! my $x = "a"; say ~("xb" ~~ / x { $x = "b" } $x /);   # xb
//! ```
//!
//! Rakudo reads an interpolated scalar when the cursor reaches it, so a code
//! block earlier in the pattern can change what the atom matches. Splicing the
//! value into the pattern text before the match starts cannot see that write.
//! A pattern reads every plain scalar it names at match time: a code block can
//! assign it, a loop re-enters atoms written before the block, and without a
//! block one body per pattern replaces a pattern text per value.
//! Those scalars are declared as match-time lexicals (the same declaration block a
//! rule's parameters use, see `regex_rule_params`), which the interpolation
//! pre-pass leaves in the text and the structural parser lowers to a
//! `RegexAtom::VarInterp`.

/// The plain scalar names the pattern `text` writes, in order of first use, and
/// whether it contains a code block. A name a `=` follows is an alias
/// (`$x=ATOM`, dropped by the interpolation pre-pass) and is left out.
// Cost: O(n), n = the text's length.
pub(crate) fn scalars_in_pattern(text: &str) -> (Vec<String>, bool) {
    let (names, saw_block) = scan_scalars(text);
    (names.into_iter().map(|(name, _)| name).collect(), saw_block)
}

/// Every plain scalar name in order of first use, flagged by whether it was
/// written after a code block, and whether the text had a code block at all.
fn scan_scalars(text: &str) -> (Vec<(String, bool)>, bool) {
    if !text.contains('$') {
        return (Vec::new(), false);
    }
    let chars: Vec<char> = text.chars().collect();
    let mut names: Vec<(String, bool)> = Vec::new();
    let mut seen_block = false;
    let mut i = 0usize;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '\'' | '"' => {
                let quote = chars[i];
                i += 1;
                while i < chars.len() && chars[i] != quote {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            '#' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '<' if matches!(chars.get(i + 1), Some('[' | '-' | '+')) => {
                // A character class: its braces and dollars are plain members.
                while i < chars.len() && chars[i] != '>' {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            '{' => {
                seen_block = true;
                let mut depth = 1usize;
                i += 1;
                while i < chars.len() && depth > 0 {
                    match chars[i] {
                        '\\' => i += 1,
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
            }
            '$' if chars
                    .get(i + 1)
                    .is_some_and(|c| c.is_alphabetic() || *c == '_') =>
            {
                let start = i + 1;
                let mut j = start;
                while j < chars.len() {
                    let ch = chars[j];
                    let kebab = ch == '-'
                        && chars
                            .get(j + 1)
                            .is_some_and(|n| n.is_alphabetic() || *n == '_');
                    if ch.is_alphanumeric() || ch == '_' || kebab {
                        j += 1;
                    } else {
                        break;
                    }
                }
                let name: String = chars[start..j].iter().collect();
                let mut k = j;
                while chars.get(k).is_some_and(|c| c.is_whitespace()) {
                    k += 1;
                }
                if chars.get(k) == Some(&'=') && !matches!(chars.get(k + 1), Some('=' | '>')) {
                    i = j;
                    continue;
                }
                // The atom of an alias (`$<a>=$re`) files a capture of its own,
                // which the interpolation atom does not.
                let mut b = i;
                while b > 0 && chars[b - 1].is_whitespace() {
                    b -= 1;
                }
                if b > 0 && chars[b - 1] == '=' && !matches!(chars.get(b.wrapping_sub(2)), Some('=' | '!' | '<')) {
                    i = j;
                    continue;
                }
                match names.iter_mut().find(|(n, _)| *n == name) {
                    // Written after a block anywhere: late.
                    Some(entry) => entry.1 |= seen_block,
                    None => names.push((name, seen_block)),
                }
                i = j;
            }
            _ => i += 1,
        }
    }
    (names, seen_block)
}

impl super::super::Interpreter {
    /// Whether a `$name` after a code block should be read at match time: the
    /// variable is a lexical scalar holding a plain value, a list (matched as its
    /// string), a junction (an alternation) or a regex, which `VarInterp`
    /// matches exactly as the splice did. Anything else (a callable, an unbound
    /// name) keeps the splice path.
    // Cost: O(1).
    pub(crate) fn late_scalar_reads_at_match_time(&self, name: &str, allow_regex: bool) -> bool {
        use crate::value::ValueView;
        let value = self
            .env
            .get(name)
            .or_else(|| self.env.get(&format!("${name}")))
            .cloned()
            .map(crate::value::Value::into_deref);
        value.is_some_and(|v| {
            matches!(
                v.view(),
                ValueView::Str(_)
                    | ValueView::Int(_)
                    | ValueView::Num(_)
                    | ValueView::Array(..)
                    | ValueView::Junction { .. }
                    | ValueView::Nil
            ) || (allow_regex
                && matches!(v.view(), ValueView::Regex(_) | ValueView::RegexWithAdverbs(_)))
        })
    }
}

impl super::super::Interpreter {
    /// Whether an interpolated `$name` may hold text the case-folding match
    /// has to expand. A Regex object never does: it keeps its own adverbs, so
    /// the enclosing literal's `:i` does not fold it, and folding the subject
    /// for it would break its exact-case literals (`m:i{^$re}` with
    /// `$re = rx{A}` matches "A").
    // Cost: O(1).
    pub(crate) fn interp_var_may_fold(&self, name: &str) -> bool {
        use crate::value::ValueView;
        let value = self
            .env
            .get(name)
            .or_else(|| self.env.get(&format!("${name}")))
            .cloned()
            .map(crate::value::Value::into_deref);
        !value.is_some_and(|v| match v.view() {
            ValueView::Regex(_) => true,
            ValueView::RegexWithAdverbs(a) => !a.ignore_case,
            _ => false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::scalars_in_pattern;

    #[test]
    fn every_scalar_of_a_pattern_is_read_at_match_time() {
        assert_eq!(scalars_in_pattern("$a { 1 } $b").0, ["a", "b"]);
        assert_eq!(scalars_in_pattern("$a $b").0, ["a", "b"]);
        assert!(scalars_in_pattern("$a=$b").0.is_empty());
        assert!(scalars_in_pattern("{ $b } '$q' \\$e <[$f]>").0.is_empty());
        assert_eq!(scalars_in_pattern("$a { 1 } $a $c").0, ["a", "c"]);
    }
}
