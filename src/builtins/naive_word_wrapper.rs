//! `Str.naive-word-wrapper`: rakudo's implementation-detail word wrapper.
//!
//! Rakudo's core `Str` carries this method (`src/core.c/Str.rakumod`) for its
//! own diagnostics, and vendored core modules call it: upstream
//! `NativeCall.rakumod` and `NativeCall/Types.rakumod` format their error
//! messages with `.naive-word-wrapper` (ADR-11203, #11208). The algorithm below
//! is upstream's, step for step, including its quirks: the first word of a line
//! is counted with a leading separator, and a word that overflows an empty line
//! becomes a line of its own.

use crate::builtins::grapheme_index::GraphemeIndex;
use crate::value::{RuntimeError, Value, ValueView};

/// Upstream's default `:max`.
const DEFAULT_MAX: i64 = 72;

/// Graphemes in `s`, the unit rakudo's `.chars` counts.
// Cost: O(n), n = bytes of `s`.
fn graphemes(s: &str) -> i64 {
    GraphemeIndex::build(s).len() as i64
}

/// `word` with every SGR escape (`ESC [ <digits> m`) removed, so colour codes
/// do not count towards the visible width.
// Cost: O(n), n = bytes of `word`.
fn strip_sgr(word: &str) -> std::borrow::Cow<'_, str> {
    if !word.contains('\u{1b}') {
        return std::borrow::Cow::Borrowed(word);
    }
    let mut out = String::with_capacity(word.len());
    let mut rest = word;
    while let Some(esc) = rest.find('\u{1b}') {
        out.push_str(&rest[..esc]);
        let after = &rest[esc + 1..];
        let digits = after
            .strip_prefix('[')
            .map(|t| t.len() - t.trim_start_matches(|c: char| c.is_ascii_digit()).len());
        match digits {
            Some(n) if n > 0 && after[1 + n..].starts_with('m') => {
                rest = &after[1 + n + 1..];
            }
            _ => {
                out.push('\u{1b}');
                rest = after;
            }
        }
    }
    out.push_str(rest);
    std::borrow::Cow::Owned(out)
}

/// Wrap `text`'s words into lines no wider than `max` graphemes (escape codes
/// excluded), each prefixed with `indent`, joined with newlines.
// Cost: O(n), n = bytes of `text`.
pub(crate) fn naive_word_wrapper(text: &str, max: i64, indent: &str) -> String {
    let indent_width = graphemes(indent);
    let mut lines: Vec<String> = Vec::new();
    let mut line: Vec<&str> = Vec::new();
    let mut width = indent_width;
    for word in text.split_whitespace() {
        let visible = graphemes(&strip_sgr(word));
        if width + visible >= max {
            if line.is_empty() {
                lines.push(format!("{indent}{word}"));
                width = indent_width;
            } else {
                lines.push(format!("{indent}{}", line.join(" ")));
                line = vec![word];
                width = indent_width + visible;
            }
        } else {
            line.push(word);
            width += 1 + visible;
        }
    }
    if !line.is_empty() {
        lines.push(format!("{indent}{}", line.join(" ")));
    }
    lines.join("\n")
}

/// The method form: a defined `Str` invocant plus the named arguments
/// `:max` and `:indent` (any other named argument is dropped before dispatch by
/// `accepted_nameds`). `None` for anything else, which leaves the call to
/// ordinary dispatch and its error.
// Cost: O(n), n = bytes of the invocant.
pub(crate) fn native_naive_word_wrapper(
    target: &Value,
    nameds: &[&Value],
) -> Option<Result<Value, RuntimeError>> {
    if !target.is_str_value() {
        return None;
    }
    let mut max = DEFAULT_MAX;
    let mut indent = String::new();
    for arg in nameds {
        let ValueView::Pair(key, value) = arg.view() else {
            return None;
        };
        match key.as_str() {
            "max" => max = crate::runtime::to_int(value),
            "indent" => indent = value.to_string_value(),
            _ => return None,
        }
    }
    let text = target.to_string_value();
    Some(Ok(Value::str(naive_word_wrapper(&text, max, &indent))))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wraps_at_the_limit() {
        assert_eq!(naive_word_wrapper("aaa bbb ccc", 8, ""), "aaa bbb\nccc");
        assert_eq!(naive_word_wrapper("abc def ghi", 72, ""), "abc def ghi");
    }

    #[test]
    fn indents_every_line() {
        assert_eq!(
            naive_word_wrapper("aaa bbb ccc", 10, "  "),
            "  aaa bbb\n  ccc"
        );
    }

    #[test]
    fn an_overlong_first_word_is_its_own_line() {
        assert_eq!(naive_word_wrapper("abcdefghij k", 5, ""), "abcdefghij\nk");
    }

    #[test]
    fn colour_codes_do_not_count() {
        assert_eq!(strip_sgr("\u{1b}[31mred\u{1b}[0m"), "red");
        assert_eq!(strip_sgr("a\u{1b}x"), "a\u{1b}x");
    }
}
