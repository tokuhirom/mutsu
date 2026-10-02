//! Rakudo's `Str.naive-word-wrapper`, which several core exception messages
//! (`X::TypeCheck`'s explanation, `X::Buf::AsStr`, `X::Adverb`) are passed
//! through. A leaf module: the pure error constructors in `value` use it too.

/// Rakudo's `Str.naive-word-wrapper(:$max)` (no indent), reproduced exactly,
/// quirks included, so a wrapped message breaks where rakudo's does:
///
/// - the running width starts at 0 and every appended word adds `1 + chars`,
///   so the FIRST line holds at most `max - 1` characters while a line begun
///   by a wrap (whose width restarts at the word's own length) holds `max`;
/// - a word that alone reaches `max` on an empty line becomes a line of its
///   own.
///
/// Words are split on any whitespace, as `.words` does.
// Cost: O(n), n = length of `text`.
pub(crate) fn naive_word_wrap(text: &str, max: usize) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut line: Vec<&str> = Vec::new();
    let mut width = 0usize;
    for word in text.split_whitespace() {
        let visible = word.chars().count();
        if width + visible >= max {
            if line.is_empty() {
                lines.push(word.to_string());
                width = 0;
            } else {
                lines.push(line.join(" "));
                line = vec![word];
                width = visible;
            }
        } else {
            line.push(word);
            width += 1 + visible;
        }
    }
    if !line.is_empty() {
        lines.push(line.join(" "));
    }
    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::naive_word_wrap;

    #[test]
    fn first_line_is_one_column_shorter_than_later_ones() {
        // 71 visible columns fit on the first line, 72 do not.
        let a = "a".repeat(35);
        let b = "b".repeat(35);
        assert_eq!(naive_word_wrap(&format!("{a} {b}"), 72), format!("{a} {b}"));
        let b36 = "b".repeat(36);
        assert_eq!(
            naive_word_wrap(&format!("{a} {b36}"), 72),
            format!("{a}\n{b36}")
        );
        // A line started by a wrap takes 72.
        let c = "c".repeat(36);
        let d = "d".repeat(35);
        assert_eq!(
            naive_word_wrap(&format!("{} {c} {d}", "x".repeat(70)), 72),
            format!("{}\n{c} {d}", "x".repeat(70))
        );
    }

    #[test]
    fn an_overlong_word_on_an_empty_line_stands_alone() {
        let long = "w".repeat(80);
        assert_eq!(
            naive_word_wrap(&format!("{long} x"), 72),
            format!("{long}\nx")
        );
    }
}
