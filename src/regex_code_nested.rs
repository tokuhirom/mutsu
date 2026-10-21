//! A regex's embedded code block (`{ … }`, `<{ … }>`, `<?{ … }>`): where it
//! ends, and a regex literal nested inside it.

/// The index of the `}` closing a code block whose body `rest` starts right
/// after the opening `{`, or `None` when the block never closes. Every regex
/// scanner that has to find the end of a code block asks this one, so they
/// cannot disagree about where it ends.
///
/// Inside the body a brace counts only as code: braces inside a quoted
/// string (`{ say "a}b" }`), a `#` line comment, or a nested regex literal
/// (`{ $s ~~ / '}' / }`) do not. An apostrophe opens no string inside a
/// comment (`# Don't …`) or an identifier (`don't`, which Raku allows
/// between two letters) (#13928). Embedded comments use the same delimiter
/// scanner as main-slang whitespace, including repeated and Unicode brackets.
// Cost: O(n), n = chars scanned.
pub(crate) fn code_block_body_len(rest: &[char]) -> Option<usize> {
    let mut depth = 1usize;
    // The last non-whitespace code character, to tell a nested regex
    // literal's opening `/` from a division.
    let mut prev_sig = '{';
    let mut i = 0usize;
    while i < rest.len() {
        let ch = rest[i];
        match ch {
            '\\' => {
                prev_sig = ch;
                i += 2;
                continue;
            }
            '#' => {
                i = comment_end(rest, i)?;
                continue;
            }
            '/' if slash_opens_regex_after(prev_sig) => {
                let len = nested_regex_literal_len(&rest[i + 1..])?;
                prev_sig = '/';
                i += 1 + len;
                continue;
            }
            '<' if starts_enumerated_class(rest, i) => {
                i = enumerated_class_end(rest, i)?;
                prev_sig = '>';
                continue;
            }
            '\'' if i > 0
                && rest[i - 1].is_alphanumeric()
                && rest.get(i + 1).is_some_and(|next| next.is_alphabetic()) => {}
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {
                if let Some(closer) = crate::regex_qq_atoms::quote_closer(ch) {
                    i = quoted_end(rest, i)? + 1;
                    prev_sig = closer;
                    continue;
                }
            }
        }
        if !ch.is_whitespace() {
            prev_sig = ch;
        }
        i += 1;
    }
    None
}

// Cost: O(n), n = comment length; O(1) auxiliary space.
fn comment_end(source: &[char], start: usize) -> Option<usize> {
    let marker = source.get(start + 1);
    let bracketed = marker == Some(&'\x60')
        || matches!(marker, Some('|' | '='))
            && source
                .get(start + 2)
                .copied()
                .and_then(crate::source_comments::matching_bracket)
                .is_some();
    if bracketed {
        let span = crate::source_comments::bracketed_comment_len(source[start..].iter().copied())?;
        return Some(start + span.chars);
    }
    let mut end = start;
    while end < source.len() && source[end] != '\n' {
        end += 1;
    }
    Some(end)
}

// Cost: O(n), n = quoted source length; O(1) auxiliary space.
fn quoted_end(source: &[char], start: usize) -> Option<usize> {
    let open = source[start];
    let mut end = start + 1;
    while let Some(&ch) = source.get(end) {
        if crate::regex_qq_atoms::quote_closes(open, ch) {
            return Some(end);
        }
        end += if ch == '\\' && open != '\u{FF62}' {
            2
        } else {
            1
        };
    }
    None
}

// Cost: O(1).
fn starts_enumerated_class(source: &[char], start: usize) -> bool {
    source.get(start + 1) == Some(&'[')
        || matches!(source.get(start + 1), Some('-' | '+' | '!'))
            && source.get(start + 2) == Some(&'[')
}

// Cost: O(n), n = enumerated class length; O(1) auxiliary space.
fn enumerated_class_end(source: &[char], start: usize) -> Option<usize> {
    let mut end = start + 2;
    while end < source.len() {
        if source[end] == '\\' {
            end += 2;
        } else if source[end] == ']' && source.get(end + 1) == Some(&'>') {
            return Some(end + 2);
        } else {
            end += 1;
        }
    }
    None
}

/// Does a `/` right after code character `prev` open a regex literal rather
/// than divide? It does in term position: after an infix or prefix operator
/// character or an opening bracket or separator. (`$/` is not reached: its
/// `$` is not in the set.)
// Cost: O(1).
pub(crate) fn slash_opens_regex_after(prev: char) -> bool {
    matches!(
        prev,
        '~' | '=' | '(' | ',' | '{' | '[' | '|' | '&' | '!' | '?' | ':' | ';'
    )
}

/// The length, in chars, of a nested `/ … /` regex literal's body plus its
/// closing `/`, where `rest` starts right after the opening `/`. A character
/// class (`<[ … ]>`) is literal text, so a quote in it (`<["']>`) does not open
/// a string; outside a class a quoted string is skipped whole, and `\` escapes
/// the next character. `None` when the literal never closes.
// Cost: O(n), n = chars scanned.
pub(crate) fn nested_regex_literal_len(rest: &[char]) -> Option<usize> {
    let mut i = 0usize;
    while i < rest.len() {
        match rest[i] {
            '\\' => i += 2,
            '/' => return Some(i + 1),
            '#' => i = comment_end(rest, i)?,
            '<' if starts_enumerated_class(rest, i) => i = enumerated_class_end(rest, i)?,
            ch if crate::regex_qq_atoms::quote_closer(ch).is_some() => i = quoted_end(rest, i)? + 1,
            _ => i += 1,
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_body(source: &str) {
        let chars: Vec<_> = source.chars().collect();
        let end = code_block_body_len(&chars).expect("the code block closes");
        assert_eq!(chars[end], '}');
        assert_eq!(
            chars[..end].iter().collect::<String>(),
            source.strip_suffix("}tail").unwrap()
        );
    }

    #[test]
    fn braces_in_comments_and_quotes_are_not_code() {
        for source in [
            " #\x60( } don't \" ) 7 }tail",
            " #|{ text { nested } } 7 }tail",
            " #\x60{{ text {{ nested }} }} 7 }tail",
            " \u{201c}}\u{201d} }tail",
            " \u{ff62}\\\u{ff63} }tail",
            " don't(); 7 }tail",
        ] {
            check_body(source);
        }
    }

    #[test]
    fn nested_regexes_keep_their_own_literals_and_comments() {
        for source in [
            " my $x = <[ } ]>; 7 }tail",
            " my $m = / <[{]> /; 7 }tail",
            " my $m = / a # } don't / ignored\n b /; 7 }tail",
            " my $m = / \u{201c}/\u{201d} /; 7 }tail",
        ] {
            check_body(source);
        }
    }

    #[test]
    fn missing_delimiters_fail_without_rescanning_suffixes() {
        for source in [" #\x60(unclosed }", " my $r = /unclosed", " \"unclosed }"] {
            let chars: Vec<_> = source.chars().collect();
            assert!(code_block_body_len(&chars).is_none());
        }
    }
}
