//! Heredocs started inside a regex code block.
//!
//! In `token { [ { die Q:to<ERR> } ]**1..* ⏎ ⋯ text ⋯ ⏎ ERR ⏎ x }` the heredoc's
//! body lines follow the line its code block closes on, so in the regex text they
//! sit *outside* the block's braces, where a regex parser would read them as
//! atoms. Rakudo's grammar reads them as the heredoc's body. This module moves
//! each such body (through its terminator line) inside the block, ahead of the
//! closing `}`, so every later reader — the RakuAST regex tree and the matcher —
//! sees an ordinary self-contained code block.

use crate::parser::primary::string::heredoc_delimiter_after;

/// `pattern` with every heredoc body that belongs to a regex code block moved
/// inside that block. The number of newlines is unchanged, so a line computed
/// from the text after the fold is still right. Borrowed when no block starts a
/// heredoc.
// Cost: O(n·b), n = pattern length, b = code blocks that start a heredoc (each
// fold rescans the text after it).
pub(crate) fn fold_code_block_heredocs(pattern: &str) -> std::borrow::Cow<'_, str> {
    if !pattern.contains(":to") && !pattern.contains(":heredoc") {
        return std::borrow::Cow::Borrowed(pattern);
    }
    let mut text = pattern.to_string();
    let mut from = 0usize;
    while let Some((open, close)) = next_code_block(&text, from) {
        from = match fold_one(&mut text, open, close) {
            Some(next) => next,
            None => close + 1,
        };
    }
    std::borrow::Cow::Owned(text)
}

/// The byte offsets of the `{` and its matching `}` of the next regex code
/// block at or after `from`, skipping escapes, quoted literals and
/// `<[ … ]>` character classes.
// Cost: O(n), n = bytes scanned.
fn next_code_block(text: &str, from: usize) -> Option<(usize, usize)> {
    let mut iter = text[from..].char_indices().map(|(i, c)| (i + from, c));
    while let Some((i, c)) = iter.next() {
        match c {
            '\\' => {
                iter.next();
            }
            '\'' | '"' => {
                for (_, q) in iter.by_ref() {
                    if q == c {
                        break;
                    }
                }
            }
            '<' if text[i + 1..].starts_with('[') => {
                let mut prev = '\0';
                for (_, q) in iter.by_ref() {
                    if prev == ']' && q == '>' {
                        break;
                    }
                    prev = if prev == '\\' { '\0' } else { q };
                }
            }
            '{' => {
                let chars: Vec<char> = text[i + 1..].chars().collect();
                let len = crate::regex_code_nested::code_block_body_len(&chars)?;
                let close = i + 1 + chars[..len].iter().map(|c| c.len_utf8()).sum::<usize>();
                return Some((i, close));
            }
            _ => {}
        }
    }
    None
}

/// Fold the heredoc bodies of the block `text[open..=close]`, if it starts
/// any whose body follows the block's line. Returns the offset to continue
/// scanning from.
// Cost: O(n), n = bytes after the block.
fn fold_one(text: &mut String, open: usize, close: usize) -> Option<usize> {
    let code = &text[open + 1..close];
    let delimiters = heredoc_delimiters(code);
    if delimiters.is_empty() {
        return None;
    }
    // A body the block already holds (the heredoc's line ends inside it) is
    // not this module's case.
    if code.contains('\n') {
        return None;
    }
    let line_end = close + text[close..].find('\n')?;
    let mut pos = line_end + 1;
    for delimiter in &delimiters {
        pos = terminator_line_end(text, pos, delimiter)?;
    }
    // `pos` is the end of the last terminator line (at its `\n`, or the end).
    let region = text[line_end + 1..pos].to_string();
    let rest_of_line = text[close + 1..line_end].to_string();
    let after = &text[pos..];
    // The newline that ended the last terminator line becomes a space, so the
    // count of newlines stays the same.
    let after = match after.strip_prefix('\n') {
        Some(tail) => format!(" {tail}"),
        None => after.to_string(),
    };
    let mut folded = String::with_capacity(text.len() + 2);
    folded.push_str(&text[..close]);
    folded.push('\n');
    folded.push_str(&region);
    folded.push('\n');
    let next = folded.len() + 1;
    folded.push('}');
    folded.push_str(&rest_of_line);
    folded.push_str(&after);
    *text = folded;
    Some(next)
}

/// The delimiters of the heredocs `code` starts, in order (`Q:to<ERR>`,
/// `q:to/END/`, `qq:heredoc[X]`).
// Cost: O(n), n = bytes of the code.
fn heredoc_delimiters(code: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut at = 0usize;
    while let Some(found) = code[at..].find(':') {
        let start = at + found + 1;
        at = start;
        let rest = &code[start..];
        let after = if let Some(r) = rest.strip_prefix("heredoc") {
            r
        } else if let Some(r) = rest.strip_prefix("to") {
            r
        } else {
            continue;
        };
        if after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-') {
            continue;
        }
        if let Some(delimiter) = heredoc_delimiter_after(after) {
            out.push(delimiter);
        }
    }
    out
}

/// The end of the terminator line of `delimiter` at or after `from` (the
/// offset of its `\n`, or the end of `text`).
// Cost: O(n), n = bytes scanned.
fn terminator_line_end(text: &str, from: usize, delimiter: &str) -> Option<usize> {
    let mut line_start = from;
    while line_start <= text.len() {
        let line_end = text[line_start..]
            .find('\n')
            .map_or(text.len(), |nl| line_start + nl);
        let line = text[line_start..line_end].trim_start_matches([' ', '\t']);
        if let Some(tail) = line.strip_prefix(delimiter)
            && (tail.is_empty() || tail.starts_with(';') || tail.starts_with('\r'))
        {
            return Some(line_end);
        }
        if line_end == text.len() {
            return None;
        }
        line_start = line_end + 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::fold_code_block_heredocs;

    #[test]
    fn moves_a_body_inside_its_block() {
        let src = "[ { die Q:to<E> } ]* \n  body\n  E\n x ";
        let folded = fold_code_block_heredocs(src);
        assert_eq!(folded, "[ { die Q:to<E> \n  body\n  E\n} ]*   x ");
        assert_eq!(folded.matches('\n').count(), src.matches('\n').count());
    }

    #[test]
    fn leaves_other_patterns_alone() {
        let src = "a { say 'x:to' } b";
        assert_eq!(fold_code_block_heredocs(src), src);
        let src = "'{ Q:to<E> }' \n E\n";
        assert_eq!(fold_code_block_heredocs(src), src);
    }
}
