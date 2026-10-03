//! `rule` sigspace before a quantifier: `<item> +` matches whitespace after
//! every repetition, as rakudo does — the whitespace binds to the quantified
//! atom (`[<item> <.ws>]+`), not to whatever follows the quantifier (#10569).

/// Whether a whitespace run followed by `next` sits between an atom and its
/// quantifier.
pub(super) fn is_quantifier_start(next: Option<char>) -> bool {
    matches!(next, Some('*' | '+' | '?'))
}

/// Mark the term that ends `out` as backtrackable (`:!`) when the rule's
/// significant whitespace follows it. Rakudo ratchets each term of a sequence
/// on its outermost node, and a term with significant whitespace after it is
/// the `[term <.ws>]` wrapper, so the ratchet lands on the wrapper and the
/// term itself can still give its match back: `rule { 'D' <id>? <v> }` gives
/// `<id>` back to let `<v>` match (ASN::Grammar's `'DEFAULT' <id-string>?
/// <value>`), and `rule { '(' [ <x>? || <y> ] ')' }` reaches `<y>` (#11162).
/// The terms marked are the ones that can backtrack: a quantified atom, a
/// group, a capture or a subrule call. An explicit backtrack modifier (`?:`,
/// `?:!`, `??`, `?!`) keeps its own meaning, a `%` separator binds to the
/// quantifier, and a sigil alias (`$<x>=[ … ]`) ratchets the atom it binds
/// itself, as rakudo does.
// Cost: O(n), n = length of `out` (one backward scan for the atom start).
pub(super) fn mark_backtracking_before_ws(out: &mut String, next: Option<char>, escaped: bool) {
    // Nothing follows inside this rule (`… <x>? }`): there is nothing to give
    // an element back to, and the rule itself returns ratcheted.
    if escaped || next.is_none() || next == Some('%') {
        return;
    }
    let trimmed_len = out.trim_end().len();
    let Some(atom_end) = backtrackable_term_atom_end(&out[..trimmed_len]) else {
        return;
    };
    if let Some(start) = last_atom_start(&out[..atom_end])
        && (is_sigil_alias_target(&out[..start]) || is_separator_atom(&out[..start]))
    {
        return;
    }
    out.truncate(trimmed_len);
    out.push_str(":!");
}

/// If `text` ends with a term that can backtrack, the byte offset where the
/// term's atom ends (before any quantifier).
fn backtrackable_term_atom_end(text: &str) -> Option<usize> {
    // `<id> ** 0..1`: a range quantifier, which the single-char check below
    // cannot see.
    if let Some(pos) = text.rfind("**") {
        let range = text[pos + 2..].trim_start();
        if !range.is_empty()
            && range
                .chars()
                .all(|c| c.is_ascii_digit() || matches!(c, '.' | '^' | '*'))
        {
            return Some(text[..pos].trim_end().len());
        }
    }
    let mut rev = text.chars().rev();
    let (Some(last), Some(before)) = (rev.next(), rev.next()) else {
        return None;
    };
    if matches!(last, '?' | '*' | '+') {
        // A frugal (`*?`, `??`) or already-modified quantifier is left alone,
        // and the character before must close an atom (`x?`, `<id>?`, `]*`,
        // `)+`).
        if matches!(before, '?' | '*' | '+' | ':' | '!' | '\\')
            || before.is_whitespace()
            || matches!(before, '|' | '&' | '(' | '[' | '{' | '<' | '%' | '=')
        {
            return None;
        }
        return Some(text.len() - last.len_utf8());
    }
    let chars: Vec<(usize, char)> = text.char_indices().collect();
    let k = chars.len() - 1;
    if is_escaped(&chars, k) {
        return None;
    }
    match last {
        ']' | ')' => Some(text.len()),
        '>' => {
            let open = matching_open(&chars, k, '<', '>')?;
            is_backtrackable_assertion(&text[chars[open].0 + 1..chars[k].0]).then_some(text.len())
        }
        _ => None,
    }
}

/// Whether the body of a `<…>` assertion is a call that can backtrack: a
/// named rule, a method or a lexical/variable regex. Lookarounds, character
/// classes, code assertions and `<.ws>` itself have a single end.
fn is_backtrackable_assertion(body: &str) -> bool {
    let name = body.trim_start_matches(['.', '&']);
    if matches!(name, "ws" | "?" | "!")
        || name.starts_with(['?', '!', '[', '-', '+', ':', '(', '{', '~'])
    {
        return false;
    }
    let ident: String = name
        .chars()
        .take_while(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':'))
        .collect();
    if matches!(
        ident.as_str(),
        "before" | "after" | "ws" | "ww" | "wb" | "same" | "at"
    ) {
        return false;
    }
    name.starts_with(|c: char| c.is_alphabetic() || matches!(c, '_' | '$' | '@'))
}

/// Whether the text before an atom ends in a `%` / `%%`, so the atom is a
/// separated quantifier's separator: part of the quantifier, not a term of the
/// sequence, and `inject_separator_ws` still has to recognize its shape.
fn is_separator_atom(before_atom: &str) -> bool {
    let trimmed = before_atom.trim_end();
    if !trimmed.ends_with('%') {
        return false;
    }
    let chars: Vec<(usize, char)> = trimmed.char_indices().collect();
    !is_escaped(&chars, chars.len() - 1)
}

/// Whether the text before an atom ends in a sigil alias's `=` (`$<x>=`,
/// `@<x>=`, `$0=`), which binds that atom.
fn is_sigil_alias_target(before_atom: &str) -> bool {
    let trimmed = before_atom.trim_end();
    if !trimmed.ends_with('=') {
        return false;
    }
    let chars: Vec<(usize, char)> = trimmed.char_indices().collect();
    !is_escaped(&chars, chars.len() - 1)
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
