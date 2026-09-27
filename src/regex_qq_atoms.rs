//! Double-quoted atoms of a regex literal (`/"x @a[0]"/`), shared by the
//! compiler and the regex interpolation pre-pass (#9628).
//!
//! A `"..."` atom follows qq-string rules: `@a[0]`, `%h<k>`, `$x.uc()`,
//! `@a.join(",")` and `{ code }` all interpolate. The match-time text
//! pre-pass (`Interpreter::interpolate_regex_scalars`) can resolve a bare
//! `$name` from `env`, but it cannot evaluate a subscript, a method call or
//! a block without re-parsing source at run time. So the compiler lowers each
//! interpolating atom through the ordinary qq-string compiler instead: the
//! atom's body becomes a compiled closure ("thunk") captured on the regex
//! value under the `MetaNs::RegexQq` env key for the body text. Installing
//! the regex's scope for a match runs the thunk, and the pre-pass splices its
//! string result in as a literal. Both sides find the atom's body with
//! [`dq_atom_close`], so they agree on the key.

/// Whether `ch` opens a double-quoted regex literal.
pub(crate) fn is_dq_opener(ch: char) -> bool {
    matches!(ch, '"' | '\u{201C}' | '\u{201E}')
}

/// The index of the quote closing the double-quoted atom opened at
/// `chars[open]`, honoring backslash escapes. A quote inside an embedded
/// `{ ... }` block or a method call's argument list (`@a.join(",")`) is
/// code, not the closer. `None` when it is unterminated.
// Cost: O(n), n = the atom's length.
pub(crate) fn dq_atom_close(chars: &[char], open: usize) -> Option<usize> {
    let mut i = open + 1;
    let mut brace_depth = 0usize;
    let mut call_depth = 0usize;
    while i < chars.len() {
        match chars[i] {
            '\\' => {
                i += 2;
                continue;
            }
            '{' => brace_depth += 1,
            '}' if brace_depth > 0 => brace_depth -= 1,
            '(' if call_depth > 0 || is_method_call_paren(chars, open, i) => call_depth += 1,
            ')' if call_depth > 0 => call_depth -= 1,
            '"' | '\u{201D}' if brace_depth == 0 && call_depth == 0 => return Some(i),
            _ => {}
        }
        i += 1;
    }
    None
}

/// Whether the `(` at `chars[at]` opens the argument list of a `.method(`
/// call (the only place a qq string's text reads an expression list).
fn is_method_call_paren(chars: &[char], open: usize, at: usize) -> bool {
    let mut j = at;
    while j > open + 1 && (chars[j - 1].is_alphanumeric() || matches!(chars[j - 1], '_' | '-')) {
        j -= 1;
    }
    j < at && j > open + 1 && chars[j - 1] == '.'
}

/// Whether a double-quoted atom's body can interpolate anything a compiled
/// thunk should evaluate. A body that reads match state (`$/`, `$0`, `$<x>`,
/// `$¢`, the topic `$_`) is excluded: that state belongs to the match in
/// progress, which a thunk run before the match cannot see.
// Cost: O(n), n = the body's length.
pub(crate) fn body_wants_thunk(body: &str) -> bool {
    let chars: Vec<char> = body.chars().collect();
    let mut interpolates = false;
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' {
            i += 2;
            continue;
        }
        if matches!(c, '$' | '@' | '%' | '&' | '{') {
            interpolates = true;
        }
        if matches!(c, '$' | '@' | '%') {
            match chars.get(i + 1) {
                Some('/' | '<' | '\u{00A2}') => return false,
                Some(d) if d.is_ascii_digit() => return false,
                Some('_') => {
                    let next = chars.get(i + 2);
                    if !next.is_some_and(|n| n.is_alphanumeric() || matches!(n, '_' | '-')) {
                        return false;
                    }
                }
                _ => {}
            }
        }
        i += 1;
    }
    interpolates
}

/// The bodies of a regex pattern's double-quoted atoms that want a compiled
/// thunk (see [`body_wants_thunk`]), deduplicated. Skips single-quoted
/// literals, character classes, embedded code blocks and comments, where a
/// `"` is not an atom opener. A pattern that declares regex-local lexicals
/// (`:my`, `:let`, ...) yields none: a body naming one would need the
/// match-time value, which the thunk cannot see.
// Cost: O(n), n = the pattern's length.
pub(crate) fn thunk_bodies(pattern: &str) -> Vec<String> {
    if [":my ", ":let ", ":our ", ":temp ", ":constant "]
        .iter()
        .any(|d| pattern.contains(d))
    {
        return Vec::new();
    }
    let chars: Vec<char> = pattern.chars().collect();
    let mut bodies: Vec<String> = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        match c {
            '\\' => i += 2,
            '#' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            '{' => i = skip_balanced(&chars, i, '{', '}'),
            '<' if matches!(chars.get(i + 1), Some('['))
                || (matches!(chars.get(i + 1), Some('-' | '+' | '?' | '!'))
                    && matches!(chars.get(i + 2), Some('['))) =>
            {
                let open = if chars[i + 1] == '[' { i + 1 } else { i + 2 };
                i = skip_balanced(&chars, open, '[', ']');
            }
            '\'' => {
                i += 1;
                while i < chars.len() && chars[i] != '\'' {
                    i += if chars[i] == '\\' { 2 } else { 1 };
                }
                i += 1;
            }
            c if is_dq_opener(c) => {
                let Some(close) = dq_atom_close(&chars, i) else {
                    break;
                };
                let body: String = chars[i + 1..close].iter().collect();
                if body_wants_thunk(&body) && !bodies.contains(&body) {
                    bodies.push(body);
                }
                i = close + 1;
            }
            _ => i += 1,
        }
    }
    bodies
}

/// The index just past the `close` matching the `open` at `chars[at]`,
/// honoring backslash escapes and nesting.
fn skip_balanced(chars: &[char], at: usize, open: char, close: char) -> usize {
    let mut depth = 0usize;
    let mut i = at;
    while i < chars.len() {
        let c = chars[i];
        if c == '\\' {
            i += 2;
            continue;
        }
        if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return i + 1;
            }
        }
        i += 1;
    }
    chars.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn finds_interpolating_dq_bodies_only() {
        assert_eq!(
            thunk_bodies(r#" "x @a[0]" 'y $z' "plain" <-["]> { "$q" } "w %h<a>" "x @a[0]" "#),
            vec!["x @a[0]".to_string(), "w %h<a>".to_string()]
        );
    }

    #[test]
    fn match_state_bodies_are_left_to_the_match() {
        assert!(!body_wants_thunk("$0"));
        assert!(!body_wants_thunk("x $<a>"));
        assert!(!body_wants_thunk("x $/"));
        assert!(!body_wants_thunk("x $_"));
        assert!(body_wants_thunk("x $_foo"));
        assert!(body_wants_thunk("x @a.join(\",\")"));
        assert!(!body_wants_thunk("plain \\$x"));
    }

    #[test]
    fn regex_local_declarations_disable_thunks() {
        assert!(thunk_bodies(r#":my $v = 1; "x $v""#).is_empty());
    }

    #[test]
    fn close_honors_escapes() {
        let chars: Vec<char> = r#""a\"b" c"#.chars().collect();
        assert_eq!(dq_atom_close(&chars, 0), Some(5));
        let chars: Vec<char> = r#""x @a.join(",")" y"#.chars().collect();
        assert_eq!(dq_atom_close(&chars, 0), Some(15));
        let chars: Vec<char> = r#""x {"a"}" y"#.chars().collect();
        assert_eq!(dq_atom_close(&chars, 0), Some(8));
        let chars: Vec<char> = r#""a (b" y"#.chars().collect();
        assert_eq!(dq_atom_close(&chars, 0), Some(5));
    }
}
