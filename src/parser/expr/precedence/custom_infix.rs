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
        // A word immediately followed by `=` (but not `==`/`=>`) is a word
        // compound assignment (`div=`, `x=`, a user `op=`), not an infix use —
        // leave it for the assignment parser so `$n div= $p` parses, including
        // as the operand of a looser operator like `and`.
        let is_compound_assign = input[end..].starts_with('=')
            && !input[end..].starts_with("==")
            && !input[end..].starts_with("=>");
        if !is_reserved_infix_word(name) && !is_declared_term && !is_compound_assign {
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
/// The precedence level at which a list-infix operator (Z/X/meta/`...`) parses
/// its operands.
///
/// - `Range`: the historical tight level (`range_expr`). Used for list-infix in
///   call-argument / feed contexts, where the loosest binding is handled by the
///   post-parse comma lift instead.
/// - `Item(mode)`: the correct "item" level (`item_expr` at `mode`), i.e.
///   everything tighter than the comma operator — comparison, `&&`, `||`, `min`,
///   item assignment, `so`/`not`, junctions. Used for the top-level list-infix
///   layer so that `1 == 1 Z 2 == 2` parses as `(1 == 1) Z (2 == 2)`.
#[derive(Clone, Copy)]
pub(crate) enum ListInfixOperand {
    Range,
    Item(crate::parser::expr::operators::ExprMode),
}

impl ListInfixOperand {
    /// Parse a single list-infix operand at this level.
    pub(crate) fn parse_single<'a>(self, input: &'a str) -> PResult<'a, Expr> {
        match self {
            ListInfixOperand::Range => range_expr(input),
            ListInfixOperand::Item(mode) => super::list_infix_top::item_expr(input, mode),
        }
    }

    /// Parse a comma-separated list of operands at this level. The comma-absorbing
    /// behaviour (which the paren/arg comma-lift later re-associates) is preserved
    /// for both levels.
    pub(crate) fn parse_comma_list<'a>(self, input: &'a str) -> PResult<'a, Vec<Expr>> {
        match self {
            ListInfixOperand::Range => parse_comma_list_of_range_raw(input),
            ListInfixOperand::Item(mode) => parse_comma_list_of_item_raw(input, mode),
        }
    }
}

/// Like [`parse_comma_list_of_range_raw`], but each element is parsed at the
/// "item" level (`item_expr`) so a list-infix operand may contain comparison /
/// logic operators (`2 == 2`, `3 && 4`).
pub(crate) fn parse_comma_list_of_item_raw<'a>(
    input: &'a str,
    mode: crate::parser::expr::operators::ExprMode,
) -> PResult<'a, Vec<Expr>> {
    let (r, first) = super::list_infix_top::item_expr(input, mode)
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
        if let Ok((r3, next)) = super::list_infix_top::item_expr(r2, mode) {
            items.push(next);
            r = r3;
        } else {
            break;
        }
    }
    Ok((r, items))
}
