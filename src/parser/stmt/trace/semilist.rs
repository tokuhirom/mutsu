//! The statement attempts of the `semilist`s inside `( )`, `[ ]`, `%( )`, a
//! hash composer and a subscript, which rakudo numbers like a block's
//! statements (see the parent module's "statement number" notes).

use super::is_unnumbered;
use crate::parser::primary::{forget_statement_attempt, record_statement_attempt};

/// The statement attempts of a `semilist`: the grammar rule behind `( ... )`,
/// `[ ... ]` and the contents of a subscript. Each of its `;`-separated
/// statements is an attempt of the same `statement` rule a block's body uses,
/// and the list ends with one more, failing, attempt at its closer -- unless it
/// is empty, which takes none (`()`, `[]`, `@a[]`). So `(1, 2)` takes two
/// numbers, `(1; 2)` three and `()` none -- while the arguments of a call
/// `f(1, 2)` are an arglist, not a semilist, and take none.
///
/// Open the list with [`semilist_open`] once the opener has been read, and
/// finish it with one of the `close` methods when the closer has
/// been found -- or [`Semilist::abandon`] it when the parse fails, so a
/// speculative attempt at something that is not a semilist leaves no number
/// behind.
pub(in crate::parser) struct Semilist<'a> {
    content: &'a str,
    /// Whether opening the list was what recorded its first attempt.
    fresh: bool,
}

/// Start a semilist from the text right after its opener. `None` for an empty
/// list, and when nothing is recording statement attempts (the common case,
/// which costs one thread-local read).
pub(in crate::parser) fn semilist_open(after_opener: &str) -> Option<Semilist<'_>> {
    if is_unnumbered() || !crate::parser::primary::records_statement_attempts() {
        return None;
    }
    let content = crate::parser::helpers::ws(after_opener).map_or(after_opener, |(rest, _)| rest);
    if content.is_empty() || content.starts_with([')', ']', '}']) {
        return None;
    }
    let (_, fresh) = record_statement_attempt(content)?;
    Some(Semilist { content, fresh })
}

impl Semilist<'_> {
    /// The list ended at its closer, which `closer_at` starts with.
    pub(in crate::parser) fn close_at(self, closer_at: &str) {
        if let Some(span) = text_before(self.content, closer_at) {
            for start in segment_starts(span) {
                let _ = record_statement_attempt(&self.content[start..]);
            }
        }
        let _ = record_statement_attempt(closer_at);
    }

    /// The list ended at a one-byte closer (`)`, `]`) that `rest` follows.
    pub(in crate::parser) fn close_after(self, rest: &str) {
        let closer = (self.content.len())
            .checked_sub(rest.len() + 1)
            .filter(|&at| self.content.is_char_boundary(at))
            .map(|at| &self.content[at..])
            .filter(|closer| text_before(closer, rest).is_some());
        match closer {
            Some(closer_at) => self.close_at(closer_at),
            None => self.abandon(),
        }
    }

    /// The parse failed: the attempt made when the list was opened is taken back.
    pub(in crate::parser) fn abandon(self) {
        if self.fresh {
            forget_statement_attempt(self.content);
        }
    }
}

/// The text of `whole` that precedes its tail `tail`, if `tail` really is the
/// end of `whole` (it can be a splice elsewhere in memory after a heredoc).
pub(super) fn text_before<'a>(whole: &'a str, tail: &str) -> Option<&'a str> {
    let same_end = tail.as_ptr() as usize + tail.len() == whole.as_ptr() as usize + whole.len();
    (same_end && tail.len() <= whole.len()).then(|| &whole[..whole.len() - tail.len()])
}

/// Where the second and later `;`-separated statements of a semilist's text
/// start (the offset of the first token after each top-level `;`). Brackets and
/// quotes are skipped over; a trailing `;` starts no statement.
fn segment_starts(span: &str) -> Vec<usize> {
    let mut starts = Vec::new();
    let mut depth = 0usize;
    let mut quote: Option<char> = None;
    let mut chars = span.char_indices();
    while let Some((at, c)) = chars.next() {
        if let Some(q) = quote {
            match c {
                '\\' => {
                    chars.next();
                }
                _ if c == q => quote = None,
                _ => {}
            }
            continue;
        }
        match c {
            '\'' | '"' => quote = Some(c),
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            ';' if depth == 0 => {
                let after = &span[at + 1..];
                let start = at + 1 + (after.len() - after.trim_start().len());
                if start < span.len() {
                    starts.push(start);
                }
            }
            _ => {}
        }
    }
    starts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn semilist_statement_starts_follow_top_level_semicolons() {
        assert_eq!(segment_starts("1; 2"), vec![3]);
        assert_eq!(segment_starts("1;"), Vec::<usize>::new());
        assert_eq!(segment_starts("(1; 2); 3"), vec![8]);
        assert_eq!(segment_starts("\"a;b\"; 'c;d'"), vec![7]);
    }
}
