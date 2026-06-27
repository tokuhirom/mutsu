use super::*;

pub(crate) fn parse_custom_infix_word(input: &str) -> Option<(String, usize)> {
    let mut word_match: Option<(String, usize)> = None;

    // Try word-like operators (alphabetic/underscore start)
    let first = input.chars().next()?;
    if first.is_alphabetic() || first == '_' {
        let mut end = first.len_utf8();
        for ch in input[end..].chars() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                end += ch.len_utf8();
            } else {
                break;
            }
        }
        let name = &input[..end];
        // A declared sigilless term symbol (e.g. `my \term = ...`) is a term, not
        // an infix operator. Without this guard, `uc term` would be misparsed as
        // `uc INFIX:<term> ...` instead of the listop call `uc(term)`.
        let is_declared_term = crate::parser::stmt::simple::match_user_declared_term_symbol(input)
            .is_some_and(|(_, consumed, _)| consumed == end);
        if !is_reserved_infix_word(name) && !is_declared_term {
            word_match = Some((name.to_string(), end));
        }
    }

    // Try user-declared symbol/mixed operators (e.g. ©, ×, _<_)
    let symbol_match = crate::parser::stmt::simple::match_user_declared_infix_symbol_op(input);

    // Prefer the longer match
    match (word_match, symbol_match) {
        (Some(w), Some(s)) => {
            if s.1 >= w.1 {
                Some(s)
            } else {
                Some(w)
            }
        }
        (Some(w), None) => Some(w),
        (None, Some(s)) => Some(s),
        (None, None) => None,
    }
}

pub(crate) fn parse_flipflop_infix(input: &str) -> Option<(String, usize)> {
    const OPS: &[&str] = &["^fff^", "^fff", "fff^", "fff", "^ff^", "^ff", "ff^", "ff"];
    for op in OPS {
        if let Some(rest) = input.strip_prefix(op)
            && !is_ident_char(rest.as_bytes().first().copied())
        {
            return Some(((*op).to_string(), op.len()));
        }
    }
    None
}

pub(crate) fn is_reserved_infix_word(name: &str) -> bool {
    if name
        .chars()
        .next()
        .map(|c| c.is_ascii_uppercase())
        .unwrap_or(false)
    {
        return true;
    }
    if name.chars().all(|c| c.is_ascii_uppercase()) {
        return true;
    }
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "with"
            | "without"
            | "my"
            | "our"
            | "state"
            | "has"
            | "sub"
            | "method"
            | "class"
            | "role"
            | "grammar"
            | "module"
            | "package"
            | "token"
            | "rule"
            | "regex"
            | "multi"
            | "proto"
            | "constant"
            | "enum"
            | "subset"
            | "unit"
            | "use"
            | "need"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "fail"
            | "take"
            | "do"
            | "try"
            | "quietly"
            | "react"
            | "whenever"
            | "loop"
            | "repeat"
            | "let"
            | "temp"
            | "where"
            | "is"
            | "does"
            | "as"
            | "of"
            | "and"
            | "or"
            | "not"
            | "xor"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "min"
            | "max"
            | "cmp"
            | "coll"
            | "unicmp"
            | "leg"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "eqv"
            | "ff"
            | "fff"
            | "after"
            | "before"
            | "gcd"
            | "lcm"
            | "x"
            | "xx"
            | "o"
    )
}

/// Parse a comma-separated list of range_expr, returning (rest, items).
pub(crate) fn parse_comma_list_of_range_raw<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    let (r, first) = range_expr(input)
        .map_err(|err| enrich_expected_error(err, "expected expression", input.len()))?;
    let mut items = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
            break;
        }
        if let Ok((r3, next)) = range_expr(r2) {
            items.push(next);
            r = r3;
        } else {
            break;
        }
    }
    Ok((r, items))
}

/// Parse a comma-separated list of range_expr, returning (rest, Vec<Expr>).
pub(crate) fn parse_comma_list_of_range<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    parse_comma_list_of_range_raw(input)
}
