//! `rule` sigspace before a quantifier: `<item> +` matches whitespace after
//! every repetition, as rakudo does — the whitespace binds to the quantified
//! atom (`[<item> <.ws>]+`), not to whatever follows the quantifier (#10569).

/// Whether a whitespace run followed by `next` sits between an atom and its
/// quantifier.
pub(super) fn is_quantifier_start(next: Option<char>) -> bool {
    matches!(next, Some('*' | '+' | '?'))
}

/// Mark a quantifier that ends `out` as backtrackable (`:!`) when the rule's
/// significant whitespace follows it. Rakudo ratchets the `[atom <.ws>]`
/// wrapper there, not the quantifier, so `rule { 'D' <id>? <v> }` gives
/// `<id>` back to let `<v>` match (ASN::Grammar's `'DEFAULT' <id-string>?
/// <value>`); an explicit backtrack modifier (`?:`, `?:!`, `??`, `?!`)
/// keeps its own meaning, and a `%` separator binds to the quantifier.
// Cost: O(1).
pub(super) fn mark_backtracking_before_ws(out: &mut String, next: Option<char>, escaped: bool) {
    if escaped || next == Some('%') {
        return;
    }
    let trimmed_len = out.trim_end().len();
    // `<id> ** 0..1`: a range quantifier, which the single-char check below
    // cannot see.
    if let Some(pos) = out[..trimmed_len].rfind("**") {
        let range = out[pos + 2..trimmed_len].trim_start();
        if !range.is_empty()
            && range
                .chars()
                .all(|c| c.is_ascii_digit() || matches!(c, '.' | '^' | '*'))
        {
            out.truncate(trimmed_len);
            out.push_str(":!");
            return;
        }
    }
    let mut rev = out[..trimmed_len].chars().rev();
    let (Some(quant), Some(before)) = (rev.next(), rev.next()) else {
        return;
    };
    let is_quant = matches!(quant, '?' | '*' | '+');
    // A frugal (`*?`, `??`) or already-modified quantifier is left alone, and
    // the character before must close an atom (`x?`, `<id>?`, `]*`, `)+`).
    if !is_quant
        || matches!(before, '?' | '*' | '+' | ':' | '!' | '\\')
        || before.is_whitespace()
        || matches!(before, '|' | '&' | '(' | '[' | '{' | '<' | '%' | '=')
    {
        return;
    }
    out.truncate(trimmed_len);
    out.push_str(":!");
}

/// Wrap the last atom of `out` as `[ATOM <.ws>]` so a quantifier that
/// follows repeats the whitespace with it. Returns false (leaving `out`
/// untouched) when the text before the quantifier does not end in an atom
/// this pass can delimit, e.g. a code block.
// Cost: O(n), n = length of `out` (one backward scan for the atom start).
pub(super) fn wrap_last_atom_with_ws(out: &mut String) -> bool {
    let trimmed_len = out.trim_end().len();
    let Some(start) = last_atom_start(&out[..trimmed_len]) else {
        return false;
    };
    let atom = out[start..trimmed_len].to_string();
    out.truncate(start);
    out.push_str("[ ");
    out.push_str(&atom);
    out.push_str(" <.ws> ]");
    true
}

/// Byte offset where the atom ending at the end of `text` begins.
fn last_atom_start(text: &str) -> Option<usize> {
    let chars: Vec<(usize, char)> = text.char_indices().collect();
    let (last_idx, last) = *chars.last()?;
    let k = chars.len() - 1;
    let start = match last {
        '>' => matching_open(&chars, k, '<', '>')?,
        ']' => matching_open(&chars, k, '[', ']')?,
        ')' => matching_open(&chars, k, '(', ')')?,
        '\'' | '"' => {
            let mut j = k;
            loop {
                j = j.checked_sub(1)?;
                if chars[j].1 == last && !is_escaped(&chars, j) {
                    break j;
                }
            }
        }
        // A code block, an anchor or a sigspace-insensitive piece of syntax
        // is not something this pass re-groups.
        '}' | '{' | '(' | '[' | '<' | '$' | '^' | '|' | '&' | ':' | '%' | '=' | '*' | '+' | '?'
        | ',' | '\\'
            if !is_escaped(&chars, k) =>
        {
            return None;
        }
        c if c.is_whitespace() => return None,
        _ => {
            if is_escaped(&chars, k) {
                k - 1
            } else {
                return Some(last_idx);
            }
        }
    };
    Some(chars[start].0)
}

/// Index of the unescaped `open` that balances the `close` at `k`.
fn matching_open(chars: &[(usize, char)], k: usize, open: char, close: char) -> Option<usize> {
    let mut depth = 0usize;
    let mut j = k;
    loop {
        let c = chars[j].1;
        if !is_escaped(chars, j) {
            if c == close {
                depth += 1;
            } else if c == open {
                depth -= 1;
                if depth == 0 {
                    return Some(j);
                }
            }
        }
        j = j.checked_sub(1)?;
    }
}

/// Whether the char at `k` is preceded by an odd run of backslashes.
fn is_escaped(chars: &[(usize, char)], k: usize) -> bool {
    chars[..k]
        .iter()
        .rev()
        .take_while(|(_, c)| *c == '\\')
        .count()
        % 2
        == 1
}

#[cfg(test)]
mod tests {
    use super::*;

    fn wrapped(text: &str) -> String {
        let mut out = text.to_string();
        wrap_last_atom_with_ws(&mut out);
        out
    }

    #[test]
    fn wraps_each_atom_shape() {
        assert_eq!(wrapped("<item> "), "[ <item> <.ws> ]");
        assert_eq!(wrapped("x 'a' "), "x [ 'a' <.ws> ]");
        assert_eq!(wrapped("[ a | b ]"), "[ [ a | b ] <.ws> ]");
        assert_eq!(wrapped("(\\d)"), "[ (\\d) <.ws> ]");
        assert_eq!(wrapped("ab"), "a[ b <.ws> ]");
        assert_eq!(wrapped("\\w"), "[ \\w <.ws> ]");
        assert_eq!(wrapped("<[<>]>"), "[ <[<>]> <.ws> ]");
    }

    #[test]
    fn leaves_code_blocks_alone() {
        assert_eq!(wrapped("{ say 1 }"), "{ say 1 }");
    }
}
