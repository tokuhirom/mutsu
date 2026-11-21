//! How a regex's internal modifier (`:i`, `:!ignorecase`, `:s`, `:r`) and a
//! double-quoted string in it (`"a\n"`) are written, for the origin walk of a
//! regex body (`regex_origins`; #15780).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};

/// The length of the internal modifier `node` written at the start of `text`:
/// a colon, a `!` exactly when the node is negated, and the modifier's short
/// or long name, not followed by more of a word. `None` when `text` does not
/// spell it.
// Cost: O(1).
pub(super) fn written_len(node: &RakuAstNode, text: &str) -> Option<usize> {
    let (short, long) = match node.class {
        RakuAstClass::RegexInternalModifierIgnoreCase => ("i", "ignorecase"),
        RakuAstClass::RegexInternalModifierIgnoreMark => ("m", "ignoremark"),
        RakuAstClass::RegexInternalModifierSigspace => ("s", "sigspace"),
        RakuAstClass::RegexInternalModifierRatchet => ("r", "ratchet"),
        _ => return None,
    };
    let negated = node.fields.iter().any(|f| {
        f.name == Some("negated") && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    });
    let rest = text.strip_prefix(':')?;
    let rest = match rest.strip_prefix('!') {
        Some(after) if negated => after,
        None if !negated => rest,
        _ => return None,
    };
    let name = [long, short].into_iter().find(|name| {
        rest.strip_prefix(name).is_some_and(|after| {
            !after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
        })
    })?;
    Some(text.len() - rest.len() + name.len())
}

/// The length of a double-quoted string's text after its opening `"` (up to
/// the closing one, excluded) and its value, when it interpolates nothing:
/// the single-character escapes (`\n`, `\t`, `\e`, `\"`, ...) are decoded,
/// and any other escape or a sigil, `&` or `{` that could interpolate gives
/// `None`.
// Cost: O(n), n = length of the string.
pub(super) fn double_quoted(inner: &str) -> Option<(usize, String)> {
    let mut value = String::new();
    let mut chars = inner.char_indices();
    while let Some((at, c)) = chars.next() {
        match c {
            '"' => return Some((at, value)),
            '$' | '@' | '%' | '&' | '{' => return None,
            '\\' => value.push(match chars.next()?.1 {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                'e' => '\u{1b}',
                'a' => '\u{7}',
                'b' => '\u{8}',
                'f' => '\u{c}',
                '0' => '\0',
                escaped @ ('\\' | '"' | '$' | '@' | '%' | '&' | '{' | '}' | ' ') => escaped,
                _ => return None,
            }),
            c => value.push(c),
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::double_quoted;

    #[test]
    fn double_quoted_escapes() {
        assert_eq!(double_quoted(r#"a\n" x"#), Some((3, "a\n".to_string())));
        assert_eq!(
            double_quoted(r#"\e[0m""#),
            Some((5, "\u{1b}[0m".to_string()))
        );
        assert_eq!(double_quoted(r#"\"q\"""#), Some((5, "\"q\"".to_string())));
        assert_eq!(double_quoted(r#"$x""#), None);
        assert_eq!(double_quoted(r#"\x41""#), None);
    }
}
