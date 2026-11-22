//! The bracketed codepoint escapes of a regex (`\x[…]`, `\o[…]`).
//!
//! As in a string, the brackets may hold a comma-separated list, which is a
//! run of codepoints: `/\x[21,21]/` matches `!!`, and whitespace around each
//! number is allowed (`\x[ 61 , 62 ]`). The run is one atom, as a quoted
//! string is (`\x[61,62]**2` matches `abab`); rakudo's `.AST` shows it as one
//! `CharClass::Specified` with all the characters.

/// Read the `[…]` after `\x` / `\o` (the `[` not yet consumed) and decode it
/// in `radix`: the codepoints, or `None` when the body is empty or a part is
/// not a number of that radix / not a codepoint.
// Cost: O(k), k = length of the bracketed body.
pub(super) fn bracketed_codepoints(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    radix: u32,
) -> Option<Vec<char>> {
    chars.next(); // '['
    let mut body = String::new();
    for ch in chars.by_ref() {
        if ch == ']' {
            break;
        }
        body.push(ch);
    }
    body.split(',')
        .map(|part| {
            u32::from_str_radix(part.trim(), radix)
                .ok()
                .and_then(char::from_u32)
        })
        .collect::<Option<Vec<char>>>()
        .filter(|codepoints| !codepoints.is_empty())
}

/// A codepoint escape's atom (`\x61`, `\x[61,62]`, `\o[141]`, `\c[NAME]`,
/// and the negated `\X`/`\O`/`\C`), made case-exact when the pattern is
/// `:i`: rakudo's `CharClass::Specified` names exact codepoints, which `:i`
/// does not fold (`"A" ~~ /:i \x61/` is `Nil`), unlike a quoted string
/// holding the same escape (#18777). Outside `:i` the atom is unchanged.
// Cost: O(1).
pub(super) fn codepoint_escape_atom(
    atom: super::RegexAtom,
    ignore_case: bool,
) -> super::RegexAtom {
    use super::{RegexAtom, RegexPattern};
    if !ignore_case {
        return atom;
    }
    match atom {
        RegexAtom::Group(mut pattern) => {
            pattern.ignore_case = false;
            RegexAtom::Group(pattern)
        }
        atom => RegexAtom::Group(RegexPattern::single_token(atom)),
    }
}
