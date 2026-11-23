//! How a regex's internal modifier (`:i`, `:!ignorecase`, `:1i`, `:i(0)`), a
//! quantifier's count and backtracking (`**:!2..4`, `**۳`), a positional
//! backreference (`$0`) and a double-quoted string in it (`"a\n"`) are
//! written, for the origin walk of a regex body (`regex_origins`; #15780).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};

/// The length of the internal modifier `node` written at the start of `text`:
/// a colon, a `!` exactly when the node is negated, and the modifier's short
/// or long name, not followed by more of a word. Without a `!` the name may
/// carry a count, before it (`:1i`) or as an argument (`:i(0)`); a zero count
/// negates. `None` when `text` does not spell it.
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
    let (bang, rest) = match rest.strip_prefix('!') {
        Some(after) => (true, after),
        None => (false, rest),
    };
    let (mut count, rest) = match (bang, written_number(rest)) {
        (false, Some((n, len))) => (Some(n), &rest[len..]),
        _ => (None, rest),
    };
    let name = [long, short].into_iter().find(|name| {
        rest.strip_prefix(name).is_some_and(|after| {
            !after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
        })
    })?;
    let mut rest = &rest[name.len()..];
    // `:i(0)`; after `:!i`, a `(` opens a capture group instead.
    if !bang
        && count.is_none()
        && let Some(inner) = rest.strip_prefix('(')
        && let Some((n, len)) = written_number(inner)
        && let Some(after) = inner[len..].strip_prefix(')')
    {
        count = Some(n);
        rest = after;
    }
    (negated == (bang || count == Some(0))).then(|| text.len() - rest.len())
}

/// The value and length of the decimal number at the start of `text`, in any
/// script's digits (`3`, `۳`), as rakudo reads a regex count.
// Cost: O(d), d = digits read.
pub(super) fn written_number(text: &str) -> Option<(u64, usize)> {
    let mut value: u64 = 0;
    let mut len = 0;
    for c in text.chars() {
        let Some(digit) = crate::ucd::numeric::unicode_decimal_digit_value(c) else {
            break;
        };
        value = value.checked_mul(10)?.checked_add(u64::from(digit))?;
        len += c.len_utf8();
    }
    (len > 0).then_some((value, len))
}

/// The length of the backtracking modifier `backtrack` written at the start
/// of `text`: `?` / `:?` (frugal), `!` / `:!` (greedy), `:` (ratchet), or
/// nothing when the quantifier has none.
// Cost: O(1).
pub(super) fn backtrack_len(
    backtrack: Option<crate::regex_tree::RegexBacktrack>,
    text: &str,
) -> Option<usize> {
    use crate::regex_tree::RegexBacktrack;
    let mark = match backtrack {
        None => return Some(0),
        Some(RegexBacktrack::Ratchet) => {
            return (text.starts_with(':') && !text[1..].starts_with(['?', '!'])).then_some(1);
        }
        Some(RegexBacktrack::Frugal) => '?',
        Some(RegexBacktrack::Greedy) => '!',
    };
    let colon = usize::from(text.starts_with(':'));
    text[colon..].starts_with(mark).then_some(colon + 1)
}

/// The length of the `** min..max` quantifier at the start of `text`, when
/// it is written with these bounds and backtracking: `**2`, `** 1..3`,
/// `**2..*`, `**:!2..4`, in any script's digits (`**۳`).
// Cost: O(n), n = length of the quantifier.
pub(super) fn range_len(
    text: &str,
    min: u64,
    max: Option<u64>,
    backtrack: Option<crate::regex_tree::RegexBacktrack>,
) -> Option<usize> {
    let blank = |at: usize| at + text[at..].len() - text[at..].trim_start().len();
    let mut at = blank(text.strip_prefix("**").map(|_| 2)?);
    // The backtracking modifier goes before the bounds.
    let mark = backtrack_len(backtrack, &text[at..])?;
    if mark > 0 {
        at = blank(at + mark);
    }
    let (low, len) = written_number(&text[at..])?;
    if low != min {
        return None;
    }
    at += len;
    match text[at..].strip_prefix("..") {
        Some(after) if after.starts_with('*') => max.is_none().then_some(at + 3),
        Some(after) => {
            let (high, len) = written_number(after)?;
            (max == Some(high)).then_some(at + 2 + len)
        }
        None => (max == Some(min)).then_some(at),
    }
}

/// The length of the positional backreference `node` (`$0`, `$٠`) written at
/// the start of `text`.
// Cost: O(d), d = digits of the index.
pub(super) fn backreference_len(node: &RakuAstNode, text: &str) -> Option<usize> {
    let index = node.fields.first().and_then(|f| match &f.value {
        RakuAstFieldValue::Node(v) => v.as_int(),
        _ => None,
    })?;
    let (n, len) = written_number(text.strip_prefix('$')?)?;
    (u64::try_from(index).ok() == Some(n)).then_some(1 + len)
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
