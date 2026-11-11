//! Copying a code block through a regex scan that tracks brackets
//! (`scan_regex_group_body`): what is inside `{ ... }` is main-slang code.

/// Copy the rest of a code block whose `{` was just copied into `body`, up
/// to and including its matching `}`: nested braces count, and a brace or
/// bracket inside a string literal of the code does not.
// Cost: O(n), n = length of the block.
pub(super) fn copy_regex_code_block(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    body: &mut String,
) {
    let mut depth = 1u32;
    let mut quote: Option<char> = None;
    let mut escaped = false;
    let mut prev = '{';
    for c in chars.by_ref() {
        body.push(c);
        let before = std::mem::replace(&mut prev, c);
        if escaped {
            escaped = false;
            continue;
        }
        match quote {
            Some(q) => {
                if c == '\\' {
                    escaped = true;
                } else if c == q {
                    quote = None;
                }
            }
            None => match c {
                '\\' => escaped = true,
                // An apostrophe inside an identifier (`isn't`) opens nothing.
                '\'' if before.is_alphanumeric() => {}
                '\'' | '"' => quote = Some(c),
                '{' => depth += 1,
                '}' => {
                    depth -= 1;
                    if depth == 0 {
                        return;
                    }
                }
                _ => {}
            },
        }
    }
}
