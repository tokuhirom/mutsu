//! Bracketed source comments shared by the main and regex code scanners.

/// Both coordinate systems used by source scanners, computed in one pass.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CommentSpan {
    pub chars: usize,
    pub bytes: usize,
}

/// Scan an embedded or declarator comment, including repeated delimiters and
/// nesting. Quotes and other bracket kinds inside the comment are plain data.
/// Each input character is consumed once; no suffix strings are allocated.
// Cost: O(n), n = comment length; O(1) auxiliary space.
pub(crate) fn bracketed_comment_len(input: impl Iterator<Item = char>) -> Option<CommentSpan> {
    let mut input = input.peekable();
    if input.next()? != '#' || !matches!(input.next()?, '\x60' | '|' | '=') {
        return None;
    }
    let open = input.next()?;
    let close = matching_bracket(open)?;
    let mut width = 1usize;
    let mut span = CommentSpan {
        chars: 3,
        bytes: 2 + open.len_utf8(),
    };
    while input.peek() == Some(&open) {
        input.next();
        width += 1;
        span.chars += 1;
        span.bytes += open.len_utf8();
    }
    let mut depth = 1usize;
    let mut previous = None;
    let mut run = 0usize;
    for ch in input {
        span.chars += 1;
        span.bytes += ch.len_utf8();
        if ch != open && ch != close {
            previous = None;
            run = 0;
            continue;
        }
        run = if previous == Some(ch) { run + 1 } else { 1 };
        previous = Some(ch);
        if run == width {
            run = 0;
            if ch == open {
                depth += 1;
            } else {
                depth -= 1;
                if depth == 0 {
                    return Some(span);
                }
            }
        }
    }
    None
}

/// The length of the whitespace and comments `text` starts with: plain `#`
/// line comments and embedded ``#`(...)`` comments. `None` when the run holds
/// what Rakudo keeps as a node of its own (Pod, a `#|` / `#=` declarator
/// comment) or an unterminated comment, so a caller placing statements around
/// it can tell it cannot.
// Cost: O(n), n = length of the run.
pub(crate) fn trivia_len(text: &str) -> Option<usize> {
    let mut at = 0;
    let mut line_start = false;
    loop {
        let rest = &text[at..];
        let blank = rest.len() - rest.trim_start().len();
        if blank > 0 {
            line_start = rest[..blank].contains('\n');
            at += blank;
            continue;
        }
        if rest.starts_with("#|") || rest.starts_with("#=") {
            return None;
        }
        if rest.starts_with("#\x60") {
            at += bracketed_comment_len(rest.chars())?.bytes;
            line_start = false;
            continue;
        }
        if rest.starts_with('#') {
            at += rest.find('\n').unwrap_or(rest.len());
            continue;
        }
        if line_start && rest.starts_with('=') {
            return None;
        }
        return Some(at);
    }
}

/// Where the `;` terminators after a statement end: `text` starts right after
/// the statement, and the answer is the offset just past its last `;`
/// (whitespace and comments before each one included), `0` when none
/// follows. `None` when [`trivia_len`] cannot read the text between them.
// Cost: O(n), n = length of the terminators and the trivia around them.
pub(crate) fn terminators_end(text: &str) -> Option<usize> {
    let (mut at, mut end) = (0, 0);
    loop {
        at += trivia_len(&text[at..])?;
        if !text[at..].starts_with(';') {
            return Some(end);
        }
        at += 1;
        end = at;
    }
}

/// Where a list of statements written in `text` ends, as a RakuAST
/// `StatementList`'s range does: its first statement starts at `first`, its
/// last ends at `last`, and the list runs on over the `;` terminators after
/// it. `None` unless everything else in `text` is whitespace and comments
/// ([`trivia_len`]): text no statement accounts for means a statement is
/// missing, and Pod is a node of its own.
// Cost: O(n), n = length of the text around the statements.
pub(crate) fn statement_list_end(text: &str, first: usize, last: usize) -> Option<usize> {
    let leading = text.get(..first)?;
    if trivia_len(leading)? != leading.len() {
        return None;
    }
    let tail = text.get(last..)?;
    let end = terminators_end(tail)?;
    (trivia_len(&tail[end..])? == tail.len() - end).then_some(last + end)
}

/// Return the matching closing bracket for an opening bracket.
// Cost: O(1).
pub(crate) fn matching_bracket(c: char) -> Option<char> {
    match c {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        '<' => Some('>'),
        '\u{00AB}' => Some('\u{00BB}'), // « »
        '\u{2018}' => Some('\u{2019}'), // ' '
        '\u{201C}' => Some('\u{201D}'), // " "
        '\u{300C}' => Some('\u{300D}'), // 「 」
        '\u{300E}' => Some('\u{300F}'), // 『 』
        '\u{FF08}' => Some('\u{FF09}'), // （ ）
        '\u{300A}' => Some('\u{300B}'), // 《 》
        '\u{3008}' => Some('\u{3009}'), // 〈 〉
        '\u{169B}' => Some('\u{169C}'), // ᚛ ᚜
        '\u{2045}' => Some('\u{2046}'), // ⁅ ⁆
        '\u{207D}' => Some('\u{207E}'), // ⁽ ⁾
        '\u{2768}' => Some('\u{2769}'), // ❨ ❩
        '\u{276E}' => Some('\u{276F}'), // ❮ ❯
        '\u{2770}' => Some('\u{2771}'), // ❰ ❱
        '\u{2772}' => Some('\u{2773}'), // ❲ ❳
        '\u{27E6}' => Some('\u{27E7}'), // ⟦ ⟧
        '\u{2985}' => Some('\u{2986}'), // ⦅ ⦆
        '\u{2993}' => Some('\u{2994}'), // ⦓ ⦔
        '\u{2995}' => Some('\u{2996}'), // ⦕ ⦖
        '\u{301D}' => Some('\u{301E}'), // 〝 〞
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trivia_and_terminators() {
        assert_eq!(trivia_len("  # c\n  #\x60(x) 1"), Some(14));
        assert_eq!(trivia_len("\n=begin pod\n"), None);
        assert_eq!(trivia_len(" #| doc\n"), None);
        assert_eq!(terminators_end(" ;; # c\n ; }"), Some(10));
        assert_eq!(terminators_end(" # c\n }"), Some(0));
        assert_eq!(terminators_end("; #= d\n;"), None);
        assert_eq!(statement_list_end(" # c\n 1 ; # d\n", 6, 7), Some(9));
        assert_eq!(statement_list_end(" 1; x", 1, 2), None);
    }

    fn remainder(source: &str) -> Option<&str> {
        let span = bracketed_comment_len(source.chars())?;
        // A char-based regex cursor and a UTF-8 parser must stop together.
        assert_eq!(
            source.chars().skip(span.chars).collect::<String>(),
            source[span.bytes..]
        );
        Some(&source[span.bytes..])
    }

    #[test]
    fn comment_contents_are_not_code() {
        assert_eq!(remainder("#\x60( } don't \" )tail"), Some("tail"));
        assert_eq!(remainder("#|[ unmatched } and ' ]tail"), Some("tail"));
        assert_eq!(remainder("#={ }tail"), Some("tail"));
    }

    #[test]
    fn repeated_delimiters_and_nesting() {
        assert_eq!(
            remainder("#\x60{ outer { inner } after }tail"),
            Some("tail")
        );
        assert_eq!(
            remainder("#\x60{{ outer {{ inner }} after }}tail"),
            Some("tail")
        );
        assert_eq!(
            remainder("#|[[ outer [[ inner ]] after ]]tail"),
            Some("tail")
        );
    }

    #[test]
    fn unicode_offsets_agree() {
        assert_eq!(
            remainder(
                "#\x60\u{ab}\u{ab} outer \u{ab}\u{ab} inner \u{bb}\u{bb} after \u{bb}\u{bb}tail"
            ),
            Some("tail")
        );
    }

    #[test]
    fn an_incomplete_close_run_is_comment_text() {
        let source = format!(
            "#\x60{}x{}x{}tail",
            "(".repeat(1024),
            ")".repeat(1023),
            ")".repeat(1024)
        );
        assert_eq!(remainder(&source), Some("tail"));
    }

    #[test]
    fn malformed_comments_do_not_produce_a_span() {
        for source in [
            "#\x60",
            "#\x60x",
            "#\x60(missing",
            "#|{missing",
            "#=missing",
            " #\x60(ok)",
        ] {
            assert!(remainder(source).is_none(), "{source}");
        }
    }
}
