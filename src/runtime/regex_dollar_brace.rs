//! `${name}` in a regex is Perl 5's braced scalar, which Raku dropped: rakudo
//! rejects it at compile time with `X::Obsolete` ("Unsupported use of ${x}.
//! In Raku please use: $x."; inside a `"..."` literal the suggestion is
//! `{$x}`). Run from the parse-time structural validator, so the error
//! surfaces when the regex is compiled, not when it first matches (#18925).

use crate::value::RuntimeError;

/// The first `${identifier}` in `pattern` that rakudo refuses, as the
/// `X::Obsolete` it raises. `$${ ... }` (the `$$` anchor before a code block),
/// a backslash-escaped `\$`, a `'...'` literal and `${ <expression> }` (not an
/// identifier) are left alone.
// Cost: O(n), n = pattern length.
pub(crate) fn check_obsolete_dollar_brace(pattern: &str) -> Result<(), RuntimeError> {
    let chars: Vec<char> = pattern.chars().collect();
    let (mut in_single, mut in_double) = (false, false);
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '\\' => {
                i += 2;
                continue;
            }
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            '$' if !in_single => {
                // `$$` is the end-of-line anchor; whatever follows it is not a
                // variable of its own.
                if chars.get(i + 1) == Some(&'$') {
                    i += 2;
                    continue;
                }
                if chars.get(i + 1) == Some(&'{')
                    && let Some(name) = braced_identifier(&chars, i + 2)
                    && let Some(err) = obsolete_braced_name(&name, in_double)
                {
                    return Err(err);
                }
            }
            _ => {}
        }
        i += 1;
    }
    Ok(())
}

/// The `X::Obsolete` for `${name}`, when `name` is an identifier (rakudo's
/// suggestion is `{$name}` inside a `"..."` literal, else `$name`).
// Cost: O(k), k = the name's length.
pub(crate) fn obsolete_braced_name(name: &str, in_double: bool) -> Option<RuntimeError> {
    let chars: Vec<char> = name.chars().chain(std::iter::once('}')).collect();
    braced_identifier(&chars, 0)?;
    let replacement = if in_double {
        format!("{{${name}}}")
    } else {
        format!("${name}")
    };
    Some(RuntimeError::obsolete(&format!("${{{name}}}"), &replacement))
}

/// The identifier spanning `chars[start..]` up to a closing `}`, when that is
/// all the braces hold.
fn braced_identifier(chars: &[char], start: usize) -> Option<String> {
    let first = *chars.get(start)?;
    if !(first.is_alphabetic() || first == '_') {
        return None;
    }
    let mut end = start + 1;
    while let Some(&c) = chars.get(end) {
        if c == '}' {
            return Some(chars[start..end].iter().collect());
        }
        let joiner = matches!(c, '-' | '\'')
            && chars
                .get(end + 1)
                .is_some_and(|n| n.is_alphabetic() || *n == '_');
        if !(c.is_alphanumeric() || c == '_' || joiner) {
            return None;
        }
        end += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::check_obsolete_dollar_brace as check;

    #[test]
    fn rejects_braced_identifiers_only() {
        assert!(check(" x ${x} ").is_err());
        assert!(check(" \"${long-name}\" ").is_err());
        assert!(check(" x $x ").is_ok());
        assert!(check(" . $${ $c = 1 } ").is_ok());
        assert!(check(" \\${x} ").is_ok());
        assert!(check(" '${x}' ").is_ok());
        assert!(check(" ${ \"a\" } ").is_ok());
    }
}
