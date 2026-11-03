//! `$var` atoms written after a code block (ADR-12917, direction 2).
//!
//! ```raku
//! my $x = "a"; say ~("xb" ~~ / x { $x = "b" } $x /);   # xb
//! ```
//!
//! Rakudo reads an interpolated scalar when the cursor reaches it, so a code
//! block earlier in the pattern can change what the atom matches. Splicing the
//! value into the pattern text before the match starts cannot see that write.
//! Before the first code block nothing can have run, so the splice and the
//! match-time read agree; from the first block on, the scalars the pattern
//! names are declared as match-time lexicals (the same declaration block a
//! rule's parameters use, see `regex_rule_params`), which the interpolation
//! pre-pass leaves in the text and the structural parser lowers to a
//! `RegexAtom::VarInterp`.

/// The plain scalar names (`$name`, no twigil) the pattern `text` writes at its
/// top level after its first code block, in order of first use. Quoted
/// literals, escapes, character classes and the code blocks themselves are not
/// scanned. Empty when the text has no code block.
// Cost: O(n), n = the text's length.
pub(crate) fn scalars_after_code_block(text: &str) -> Vec<String> {
    if !text.contains('{') {
        return Vec::new();
    }
    let chars: Vec<char> = text.chars().collect();
    let mut names: Vec<String> = Vec::new();
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
            '$' if seen_block
                && chars
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
                if !names.contains(&name) {
                    names.push(name);
                }
                i = j;
            }
            _ => i += 1,
        }
    }
    names
}

impl super::super::Interpreter {
    /// Whether a `$name` after a code block should be read at match time: the
    /// variable is a lexical scalar holding a plain value or a regex, which
    /// `VarInterp` matches exactly as the splice did. Anything else (a list, a
    /// callable, a junction, an unbound name) keeps the splice path.
    // Cost: O(1).
    pub(crate) fn late_scalar_reads_at_match_time(&self, name: &str) -> bool {
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
                    | ValueView::Nil
                    | ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs(_)
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::scalars_after_code_block;

    #[test]
    fn only_scalars_after_the_first_code_block_are_late() {
        assert_eq!(scalars_after_code_block("$a { $b = 1 } $c $c $d"), ["c", "d"]);
        assert!(scalars_after_code_block("$a $b").is_empty());
        assert!(scalars_after_code_block("{ $b } '$q' \\$e <[$f]>").is_empty());
    }
}
