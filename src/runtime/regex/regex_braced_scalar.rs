//! `${name}` in a regex is Perl 5 syntax rakudo rejects at compile time:
//! "Unsupported use of ${x}. In Raku please use: $x." (`X::Obsolete`). Inside
//! a `"..."` literal the suggested form is `{$x}` (#18925).
//!
//! `${ code }` (a space after the brace) is not this form: rakudo reads it as
//! the end anchor `$` followed by a code block, and so does mutsu.

use crate::value::RuntimeError;

/// The length of `name` in `${name}` starting at `chars[at]` (the `$`), or
/// `None` when `chars[at..]` is not that form.
// Cost: O(n), n = the name's length.
pub(crate) fn braced_scalar_name_len(chars: &[char], at: usize) -> Option<usize> {
    if chars.get(at) != Some(&'$') || chars.get(at + 1) != Some(&'{') {
        return None;
    }
    let start = at + 2;
    if !chars
        .get(start)
        .is_some_and(|c| c.is_alphabetic() || *c == '_')
    {
        return None;
    }
    let mut end = start + 1;
    while chars
        .get(end)
        .is_some_and(|c| c.is_alphanumeric() || *c == '_' || *c == '-')
    {
        end += 1;
    }
    (chars.get(end) == Some(&'}')).then_some(end - start)
}

/// The `X::Obsolete` rakudo raises for `${name}`; `in_double_quotes` picks
/// the replacement it suggests inside a `"..."` literal.
pub(crate) fn obsolete_braced_scalar(name: &str, in_double_quotes: bool) -> RuntimeError {
    let replacement = if in_double_quotes {
        format!("{{${name}}}")
    } else {
        format!("${name}")
    };
    RuntimeError::obsolete(&format!("${{{name}}}"), &replacement)
}

/// Reject a `${name}` anywhere a regex `pattern` would interpolate it: outside
/// single-quoted literals, character classes, comments and code blocks.
// Cost: O(n), n = the pattern's length.
pub(crate) fn check_obsolete_braced_scalar(pattern: &str) -> Result<(), RuntimeError> {
    if !pattern.contains("${") {
        return Ok(());
    }
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_dq = false;
    let mut i = 0usize;
    while i < chars.len() {
        let c = chars[i];
        match c {
            '\\' => {
                i += 2;
                continue;
            }
            '"' => in_dq = !in_dq,
            '\'' if !in_dq && !(i > 0 && chars[i - 1].is_alphanumeric()) => {
                i += 1;
                while i < chars.len() && chars[i] != '\'' {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
            }
            '#' if !in_dq => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '<' if !in_dq && matches!(chars.get(i + 1), Some('[' | '-' | '+')) => {
                while i < chars.len() && chars[i] != '>' {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
            }
            '$' if chars.get(i + 1) == Some(&'$') => i += 1,
            '$' => {
                if let Some(len) = braced_scalar_name_len(&chars, i) {
                    let name: String = chars[i + 2..i + 2 + len].iter().collect();
                    return Err(obsolete_braced_scalar(&name, in_dq));
                }
            }
            '{' if !in_dq => {
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
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::check_obsolete_braced_scalar as check;

    fn message(pattern: &str) -> Option<String> {
        check(pattern).err().map(|e| e.message.into_owned())
    }

    #[test]
    fn rejects_braced_scalars_where_they_interpolate() {
        assert_eq!(
            message(" x ${x} ").as_deref(),
            Some("Unsupported use of ${x}. In Raku please use: $x.")
        );
        assert_eq!(
            message(r#" x "${x}" "#).as_deref(),
            Some("Unsupported use of ${x}. In Raku please use: {$x}.")
        );
    }

    #[test]
    fn leaves_other_dollar_brace_forms_alone() {
        for pattern in [
            " x ${ say 1 } ",
            " x $ {say 1} ",
            " x '${x}' ",
            " x $${ 1 } ",
            " <[${x}]> ",
            " x { my $s = \"${x}\" } ",
            " # ${x}\n x ",
            " x \\${x} ",
        ] {
            assert!(check(pattern).is_ok(), "{pattern:?}");
        }
    }
}
