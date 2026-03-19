use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult};
use super::super::stmt::assign::parse_meta_compound_assign_op;

use crate::ast::Expr;
use crate::token_kind::TokenKind;

use super::operators::*;
use super::postfix::{postfix_expr_continue, prefix_expr};
use super::precedence::parse_custom_infix_word;

pub(super) fn parse_infix_func_op(input: &str) -> Option<(Option<String>, String, usize)> {
    let (modifier, bracket_start) = if input.starts_with("R[&") {
        (Some("R".to_string()), 1)
    } else if input.starts_with("X[&") {
        (Some("X".to_string()), 1)
    } else if input.starts_with("Z[&") {
        (Some("Z".to_string()), 1)
    } else if input.starts_with("[&") {
        (None, 0)
    } else {
        return None;
    };
    let r = &input[bracket_start + 2..]; // skip "[&"
    let end = r.find(']')?;
    let name = &r[..end];
    if name.is_empty()
        || !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return None;
    }
    let total_len = bracket_start + 2 + end + 1; // prefix + "[&" + name + "]"
    Some((modifier, name.to_string(), total_len))
}

/// Convert an operator string to its TokenKind for bracket infix notation.
pub(super) fn op_str_to_token_kind(op: &str) -> Option<TokenKind> {
    match op {
        "..." | "…" => Some(TokenKind::DotDotDot),
        "...^" | "…^" => Some(TokenKind::DotDotDotCaret),
        "+" => Some(TokenKind::Plus),
        "-" => Some(TokenKind::Minus),
        "*" => Some(TokenKind::Star),
        "/" => Some(TokenKind::Slash),
        "%" => Some(TokenKind::Percent),
        "**" => Some(TokenKind::StarStar),
        "~" => Some(TokenKind::Tilde),
        "==" => Some(TokenKind::EqEq),
        "!=" => Some(TokenKind::BangEq),
        "=:=" => Some(TokenKind::Ident("=:=".to_string())),
        "!=:=" => Some(TokenKind::Ident("!=:=".to_string())),
        "<" => Some(TokenKind::Lt),
        ">" => Some(TokenKind::Gt),
        "<=" => Some(TokenKind::Lte),
        ">=" => Some(TokenKind::Gte),
        "<=>" => Some(TokenKind::LtEqGt),
        "===" => Some(TokenKind::EqEqEq),
        "~~" => Some(TokenKind::SmartMatch),
        "&&" => Some(TokenKind::AndAnd),
        "||" => Some(TokenKind::OrOr),
        "^^" => Some(TokenKind::XorXor),
        "//" => Some(TokenKind::SlashSlash),
        "%%" => Some(TokenKind::PercentPercent),
        "+&" => Some(TokenKind::BitAnd),
        "+|" => Some(TokenKind::BitOr),
        "+^" => Some(TokenKind::BitXor),
        "~&" => Some(TokenKind::Ident("~&".to_string())),
        "~|" => Some(TokenKind::Ident("~|".to_string())),
        "~^" => Some(TokenKind::Ident("~^".to_string())),
        "(|)" | "∪" => Some(TokenKind::SetUnion),
        "(+)" | "⊎" => Some(TokenKind::SetAddition),
        "(&)" | "∩" => Some(TokenKind::SetIntersect),
        "(.)" | "⊍" => Some(TokenKind::SetMultiply),
        "(^)" | "⊖" => Some(TokenKind::SetSymDiff),
        "(elem)" | "∈" => Some(TokenKind::SetElem),
        "(cont)" | "∋" => Some(TokenKind::SetCont),
        // Word operators are represented as Ident tokens
        "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "leg" | "cmp" | "min" | "max" | "gcd" | "lcm"
        | "and" | "or" | "not" | "after" | "before" => Some(TokenKind::Ident(op.to_string())),
        _ => None,
    }
}

/// Find the index of the closing `]` that matches the opening `[` at position 0.
/// Returns the index of the matching `]`, or None if not found.
fn find_matching_bracket(input: &str) -> Option<usize> {
    let mut depth = 0;
    for (i, c) in input.char_indices() {
        match c {
            '[' => depth += 1,
            ']' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// Flatten nested bracket operator notation into a simple string.
/// e.g., "+" → "+", "R-" → "R-", "R[R-]" → "RR-", "[+]" → "+", "R[R[R-]]" → "RRR-"
fn flatten_bracket_op(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let first = s.as_bytes()[0];
    // Handle plain bracket: [op] → flatten(op)
    if first == b'['
        && let Some(end) = find_matching_bracket(s)
    {
        let inner = &s[1..end];
        return flatten_bracket_op(inner);
    }
    // Handle meta + bracket: R[op] → R + flatten(op)
    if (first == b'R' || first == b'Z' || first == b'X')
        && s.len() > 1
        && s.as_bytes()[1] == b'['
        && let Some(end) = find_matching_bracket(&s[1..])
    {
        let inner = &s[2..1 + end];
        let flattened_inner = flatten_bracket_op(inner);
        let rest = &s[1 + end + 1..];
        return format!("{}{}{}", first as char, flattened_inner, rest);
    }
    s.to_string()
}

/// Known operators for bracket infix and meta-op bracket notation.
const KNOWN_OPS: &[&str] = &[
    "...^", "...", "…^", "…", "**", "==", "!=", "=:=", "!=:=", "<=", ">=", "<=>", "===", "~~",
    "%%", "//", "||", "&&", "~&", "~|", "~^", "~", "+", "-", "*", "/", "%", "<", ">", "+&", "+|",
    "+^", "?&", "?|", "?^", "cmp", "min", "max", "eq", "ne", "lt", "gt", "le", "ge", "leg", "and",
    "or", "not", "after", "before", "gcd", "lcm", ",", "(|)", "(&)", "(.)", "(+)", "(^)", "(elem)",
    "(cont)", "∪", "∩", "⊍", "⊎", "⊖", "∈", "∋",
];

fn parse_meta_set_op(input: &str) -> Option<(String, usize)> {
    const META_SET_OPS: &[(&str, &str)] = &[
        ("(|)", "(|)"),
        ("(&)", "(&)"),
        ("(.)", "(.)"),
        ("(+)", "(+)"),
        ("(-)", "(-)"),
        ("(^)", "(^)"),
        ("(elem)", "(elem)"),
        ("(cont)", "(cont)"),
        ("∪", "∪"),
        ("∩", "∩"),
        ("⊍", "(.)"),
        ("⊎", "(+)"),
        ("∖", "(-)"),
        ("⊖", "⊖"),
        ("∈", "∈"),
        ("∋", "∋"),
    ];
    for (prefix, normalized) in META_SET_OPS {
        if input.starts_with(prefix) {
            return Some(((*normalized).to_string(), prefix.len()));
        }
    }
    None
}

/// Parse meta operator: R-, X+, Zcmp, R[+], Z[~], R[R[R-]], RR[R-], etc.
pub(super) fn parse_meta_op(input: &str) -> Option<(String, String, usize)> {
    let meta = if input.starts_with('R') {
        "R"
    } else if input.starts_with('X') {
        "X"
    } else if input.starts_with('Z') {
        "Z"
    } else {
        return None;
    };
    let r = &input[1..];

    // Try bracket notation: R[op], Z[op], X[op]
    if r.starts_with('[')
        && let Some(end) = find_matching_bracket(r)
    {
        let inner = &r[1..end];
        let op = flatten_bracket_op(inner);
        return Some((meta.to_string(), op, 1 + end + 1));
    }

    // Try chained meta with brackets: RR[op], RZ[op], etc.
    if let Some((inner_meta, inner_op, inner_len)) = parse_meta_op(r) {
        let op = format!("{}{}", inner_meta, inner_op);
        return Some((meta.to_string(), op, 1 + inner_len));
    }

    // Set operators in parenthesized and unicode form used by meta ops:
    // Z(&), Z∩, X(|), X∪, etc.
    if let Some((op, len)) = parse_meta_set_op(r) {
        return Some((meta.to_string(), op.to_string(), 1 + len));
    }

    // Try symbolic operators first (multi-char then single-char)
    let ops: &[&str] = &[
        "...^", "...", "…^", "…", "**", "=>", "==", "!=:=", "=:=", "!=", "<=", ">=", "~~", "%%",
        "//", "&&", "||", "+&", "+|", "+^", "+<", "+>", "~&", "~|", "~^", "~", "+", "-", "*", "/",
        "%", "<", ">", ",",
    ];
    for op in ops {
        if r.starts_with(op) {
            return Some((meta.to_string(), op.to_string(), 1 + op.len()));
        }
    }
    // Try word operators: cmp, min, max, eq, ne, lt, gt, le, ge, leg
    let word_ops: &[&str] = &[
        "cmp", "min", "max", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    ];
    for op in word_ops {
        if r.starts_with(op) && !is_ident_char(r.as_bytes().get(op.len()).copied()) {
            return Some((meta.to_string(), op.to_string(), 1 + op.len()));
        }
    }
    if let Some((op, len)) = parse_meta_set_op(r) {
        return Some((meta.to_string(), op, 1 + len));
    }
    // User-declared symbol infix operators: R⋅, Z⋅, X⋅, RT*, etc.
    // Try this before word ops since declared symbols may start with a letter
    // but extend with non-word chars (e.g. T* should match the declared infix,
    // not just the word portion "T").
    let user_sym = super::super::stmt::simple::match_user_declared_infix_symbol_op(r);
    // Custom word operators: Xwtf, Zfoo-bar, Rcustom-op
    let word_op = parse_meta_word_op(r);
    // Prefer the longer match between user-declared symbol and word ops
    match (user_sym, word_op) {
        (Some((sym, sym_len)), Some((word, word_len))) => {
            if sym_len >= word_len {
                return Some((meta.to_string(), sym, 1 + sym_len));
            } else {
                return Some((meta.to_string(), word, 1 + word_len));
            }
        }
        (Some((sym, sym_len)), None) => {
            return Some((meta.to_string(), sym, 1 + sym_len));
        }
        (None, Some((word, word_len))) => {
            return Some((meta.to_string(), word, 1 + word_len));
        }
        (None, None) => {}
    }
    // Bare Z (zip with comma) or bare X (cross product) — followed by non-ident, non-operator char
    if (meta == "Z" || meta == "X") && !is_ident_char(r.as_bytes().first().copied()) {
        return Some((meta.to_string(), String::new(), 1));
    }
    None
}

fn parse_meta_word_op(input: &str) -> Option<(String, usize)> {
    let first = input.chars().next()?;
    if !first.is_alphabetic() && first != '_' {
        return None;
    }
    let mut end = first.len_utf8();
    for ch in input[end..].chars() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    Some((input[..end].to_string(), end))
}

pub(super) fn strip_sequence_op(input: &str) -> Option<(&str, TokenKind, &str)> {
    if let Some(rest) = input.strip_prefix("...^") {
        Some((rest, TokenKind::DotDotDotCaret, "...^"))
    } else if let Some(rest) = input.strip_prefix("…^") {
        Some((rest, TokenKind::DotDotDotCaret, "…^"))
    } else if input.starts_with("...") && !input.starts_with("....") {
        Some((&input[3..], TokenKind::DotDotDot, "..."))
    } else if let Some(rest) = input.strip_prefix("…") {
        Some((rest, TokenKind::DotDotDot, "…"))
    } else {
        None
    }
}

/// Parse bracket infix operator: [+], [R-], [Z*], [Z[cmp]], [blue], etc.
/// Returns (kind, total_consumed_len) where kind is an enum describing
/// whether it's a plain op, meta op, or user-defined infix.
pub(super) enum BracketInfix {
    /// A plain operator like `+`, `-`, `*`, `cmp`
    PlainOp(String, usize),
    /// A meta operator like `R-`, `Z*`, `Zcmp`
    MetaOp(String, String, usize),
    /// A user-defined infix like `blue`
    UserInfix(String, usize),
}

pub(super) fn parse_bracket_infix_op(input: &str) -> Option<BracketInfix> {
    if !input.starts_with('[') {
        return None;
    }
    // Don't match [&func] (handled by parse_infix_func_op)
    if input.starts_with("[&") {
        return None;
    }
    let end = find_matching_bracket(input)?;
    let inner = &input[1..end];
    if inner.is_empty() {
        return None;
    }
    let total_len = end + 1;
    let flattened = flatten_bracket_op(inner);

    // Check if it's a meta-prefixed op
    let first = flattened.as_bytes()[0];
    if first == b'R' || first == b'Z' || first == b'X' || first == b'!' {
        let meta_ch = &flattened[..1];
        let op = &flattened[1..];
        // Make sure inner op is valid (known op or known meta pattern)
        if !op.is_empty() {
            return Some(BracketInfix::MetaOp(
                meta_ch.to_string(),
                op.to_string(),
                total_len,
            ));
        }
    }

    // Check if it's a known plain operator
    if KNOWN_OPS.contains(&flattened.as_str()) {
        return Some(BracketInfix::PlainOp(flattened, total_len));
    }

    // Check if it's a user-defined infix name (identifier)
    if flattened
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return Some(BracketInfix::UserInfix(flattened, total_len));
    }

    // Check if it matches a user-declared infix symbol operator (e.g. [T+], [⋅])
    if let Some((symbol, len)) =
        super::super::stmt::simple::match_user_declared_infix_symbol_op(inner)
        && len == inner.len()
    {
        return Some(BracketInfix::UserInfix(symbol, total_len));
    }

    None
}

fn parse_set_op(input: &str) -> Option<(TokenKind, usize)> {
    if input.starts_with("(==)") {
        Some((TokenKind::Ident("(==)".to_string()), 4))
    } else if input.starts_with('≡') {
        Some((TokenKind::Ident("≡".to_string()), '≡'.len_utf8()))
    } else if input.starts_with('≢') {
        Some((TokenKind::Ident("≢".to_string()), '≢'.len_utf8()))
    } else if input.starts_with("(|)") {
        Some((TokenKind::SetUnion, 3))
    } else if input.starts_with('∪') {
        Some((TokenKind::SetUnion, '∪'.len_utf8()))
    } else if input.starts_with("(&)") {
        Some((TokenKind::SetIntersect, 3))
    } else if input.starts_with('∩') {
        Some((TokenKind::SetIntersect, '∩'.len_utf8()))
    } else if input.starts_with("(.)") {
        Some((TokenKind::SetMultiply, 3))
    } else if input.starts_with('⊍') {
        Some((TokenKind::SetMultiply, '⊍'.len_utf8()))
    } else if input.starts_with("(+)") {
        Some((TokenKind::SetAddition, 3))
    } else if input.starts_with('⊎') {
        Some((TokenKind::SetAddition, '⊎'.len_utf8()))
    } else if input.starts_with("(-)") {
        Some((TokenKind::SetDiff, 3))
    } else if input.starts_with('∖') {
        Some((TokenKind::SetDiff, '∖'.len_utf8()))
    } else if input.starts_with("(^)") {
        Some((TokenKind::SetSymDiff, 3))
    } else if input.starts_with('⊖') {
        Some((TokenKind::SetSymDiff, '⊖'.len_utf8()))
    } else if input.starts_with("(<=)") {
        Some((TokenKind::SetSubset, 4))
    } else if input.starts_with('⊆') {
        Some((TokenKind::SetSubset, '⊆'.len_utf8()))
    } else if input.starts_with("(>=)") {
        Some((TokenKind::SetSuperset, 4))
    } else if input.starts_with('⊇') {
        Some((TokenKind::SetSuperset, '⊇'.len_utf8()))
    } else if input.starts_with("!(<)") {
        Some((TokenKind::Ident("⊄".to_string()), 4))
    } else if input.starts_with("(<)") {
        Some((TokenKind::SetStrictSubset, 3))
    } else if input.starts_with('⊂') {
        Some((TokenKind::SetStrictSubset, '⊂'.len_utf8()))
    } else if input.starts_with("(>)") {
        Some((TokenKind::SetStrictSuperset, 3))
    } else if input.starts_with('⊃') {
        Some((TokenKind::SetStrictSuperset, '⊃'.len_utf8()))
    } else if input.starts_with('⊄') {
        Some((TokenKind::Ident("⊄".to_string()), '⊄'.len_utf8()))
    } else if input.starts_with('⊅') {
        Some((TokenKind::Ident("⊅".to_string()), '⊅'.len_utf8()))
    } else if input.starts_with("(elem)") {
        Some((TokenKind::SetElem, 6))
    } else if input.starts_with('∈') {
        Some((TokenKind::SetElem, '∈'.len_utf8()))
    } else if input.starts_with("(cont)") {
        Some((TokenKind::SetCont, 6))
    } else if input.starts_with('∋') {
        Some((TokenKind::SetCont, '∋'.len_utf8()))
    } else {
        None
    }
}

/// Structural infix: but, does, set operators
pub(super) fn structural_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = concat_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with("but") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'but'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("but".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("does") && !is_ident_char(r.as_bytes().get(4).copied()) {
            let r = &r[4..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'does'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("does".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // S-metaop variants used as infix operators (e.g. S&)
        if r.starts_with("S&") && !is_ident_char(r.as_bytes().get(2).copied()) {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'S&'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("S&".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Set operators: (|), (&), (-), (^), (<=), (>=), (<), (>), (elem), (cont)
        if let Some((tok, len)) = parse_set_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after set operator", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: tok,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Parse hyper operator: >>op<<, >>op>>, <<op<<, <<op>>
/// Also supports Unicode variants: \u{00BB}op\u{00AB}, \u{00BB}op\u{00BB}, \u{00AB}op\u{00AB}, \u{00AB}op\u{00BB}
/// and mixed forms like >>op\u{00AB}, \u{00BB}op<<, etc.
fn parse_hyper_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = if let Some(r) = input.strip_prefix('\u{00BB}') {
        // \u{00BB} = >> (non-DWIM left)
        (false, '\u{00BB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        // \u{00AB} = << (DWIM left)
        (true, '\u{00AB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix(">>") {
        (false, 2, r)
    } else if let Some(r) = input.strip_prefix("<<") {
        (true, 2, r)
    } else {
        return None;
    };

    // Search for right delimiter within the operator string
    let mut search_limit = after_left.len().min(10);
    // Ensure we don't slice in the middle of a multi-byte UTF-8 character
    while search_limit > 0 && !after_left.is_char_boundary(search_limit) {
        search_limit -= 1;
    }
    let search = &after_left[..search_limit];

    // Find the earliest right delimiter (any of >>, <<, \u{00BB}, \u{00AB})
    let mut best: Option<(usize, bool, usize)> = None; // (byte_offset, dwim_right, marker_len)
    for (marker, dwim_right, marker_len) in [
        (">>", true, 2usize),
        ("<<", false, 2),
        ("\u{00BB}", true, '\u{00BB}'.len_utf8()),
        ("\u{00AB}", false, '\u{00AB}'.len_utf8()),
    ] {
        if let Some(pos) = search.find(marker)
            && pos > 0
            && (best.is_none() || pos < best.unwrap().0)
        {
            best = Some((pos, dwim_right, marker_len));
        }
    }

    if let Some((end, dwim_right, right_marker_len)) = best {
        let op = &after_left[..end];
        // Reject bare `=` as the operator — that is hyper-assignment syntax
        // (e.g. `»=»`), handled at the statement level.
        if op == "=" {
            return None;
        }
        return Some((
            op.to_string(),
            dwim_left,
            dwim_right,
            left_len + end + right_marker_len,
        ));
    }
    None
}

/// Parse hyper operator with function reference: >>[&func]<<, <<[&func]>>, etc.
/// Returns (func_name, dwim_left, dwim_right, total_consumed_length)
fn parse_hyper_func_op(input: &str) -> Option<(String, bool, bool, usize)> {
    // Determine left delimiter and dwim_left
    let (dwim_left, left_len, after_left) = if let Some(r) = input.strip_prefix('\u{00BB}') {
        (false, '\u{00BB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix('\u{00AB}') {
        (true, '\u{00AB}'.len_utf8(), r)
    } else if let Some(r) = input.strip_prefix(">>") {
        (false, 2, r)
    } else if let Some(r) = input.strip_prefix("<<") {
        (true, 2, r)
    } else {
        return None;
    };

    // Check for [&func] pattern
    if !after_left.starts_with("[&") {
        return None;
    }
    let bracket_content = &after_left[2..]; // skip "[&"
    let end = bracket_content.find(']')?;
    let name = &bracket_content[..end];
    if name.is_empty()
        || !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return None;
    }
    let after_bracket = &after_left[2 + end + 1..]; // skip "[&name]"
    let bracket_len = 2 + end + 1;

    // Determine right delimiter and dwim_right
    let (dwim_right, right_len) = if let Some(_r) = after_bracket.strip_prefix('\u{00BB}') {
        (true, '\u{00BB}'.len_utf8())
    } else if let Some(_r) = after_bracket.strip_prefix('\u{00AB}') {
        (false, '\u{00AB}'.len_utf8())
    } else if after_bracket.starts_with(">>") {
        (true, 2)
    } else if after_bracket.starts_with("<<") {
        (false, 2)
    } else {
        return None;
    };

    let total_len = left_len + bracket_len + right_len;
    Some((name.to_string(), dwim_left, dwim_right, total_len))
}

/// Try to parse a custom infix word operator at a given precedence range.
/// Returns Some(remaining_input) if a custom infix was parsed, None otherwise.
/// `min_level` is exclusive, `max_level` is inclusive.
fn try_custom_infix_at_level<'a>(
    r: &'a str,
    left: &mut Expr,
    min_level: i32,
    max_level: i32,
    operand_parser: fn(&str) -> PResult<'_, Expr>,
) -> Result<Option<&'a str>, PError> {
    if let Some((name, len)) = parse_custom_infix_word(r)
        && let Some(level) = super::super::stmt::simple::lookup_custom_infix_precedence(&name)
        && level > min_level
        && level <= max_level
    {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = operand_parser(r).map_err(|err| {
            enrich_expected_error(err, "expected expression after infix operator", r.len())
        })?;
        *left = Expr::InfixFunc {
            name: name.clone(),
            left: Box::new(left.clone()),
            right: vec![right],
            modifier: None,
        };
        return Ok(Some(r));
    }
    Ok(None)
}

/// String concatenation: ~
pub(super) fn concat_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = replication_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        // Hyper operators with function reference: >>[&func]<<, <<[&func]>>, etc.
        if let Some((func_name, dwim_left, dwim_right, len)) = parse_hyper_func_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = replication_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after hyper function operator",
                    r.len(),
                )
            })?;
            left = Expr::HyperFuncOp {
                func_name,
                left: Box::new(left),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
            rest = r;
            continue;
        }
        // Hyper operators: >>op<<, >>op>>, <<op<<, <<op>>
        if let Some((op, dwim_left, dwim_right, len)) = parse_hyper_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = replication_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after hyper operator", r.len())
            })?;
            left = Expr::HyperOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
                dwim_left,
                dwim_right,
            };
            rest = r;
            continue;
        }
        // Custom infix ops between structural and additive levels
        // (covers is equiv<~>, is tighter<~>, is looser<+>)
        {
            use super::super::stmt::simple::{PREC_ADDITIVE, PREC_STRUCTURAL};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_STRUCTURAL,
                PREC_ADDITIVE - 1,
                replication_expr,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((op, len)) = parse_pure_concat_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = replication_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after concatenation operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Replication: x, xx, o (function composition)
/// These bind tighter than ~ (concatenation) but looser than + (additive).
fn replication_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = additive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_replication_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after replication operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Classify an operator string by its precedence level.
enum OpPrecedence {
    Multiplicative,
    Additive,
    Concatenation,
    Comparison,
    Other,
}

fn classify_base_op(op: &str) -> OpPrecedence {
    // Strip all meta prefixes to find the base operator
    let mut s = op;
    while let Some(rest) = s
        .strip_prefix('R')
        .or_else(|| s.strip_prefix('Z'))
        .or_else(|| s.strip_prefix('X'))
    {
        s = rest;
    }
    match s {
        "*" | "/" | "%" | "gcd" | "lcm" | "~&" => OpPrecedence::Multiplicative,
        "+" | "-" | "~|" | "~^" => OpPrecedence::Additive,
        "~" => OpPrecedence::Concatenation,
        "==" | "!=" | "=:=" | "!=:=" | "<" | ">" | "<=" | ">=" | "<=>" | "===" | "eq" | "ne"
        | "lt" | "gt" | "le" | "ge" | "leg" | "cmp" | "~~" | "%%" => OpPrecedence::Comparison,
        _ => OpPrecedence::Other,
    }
}

/// Try to parse a bracket meta-op or bracket infix at a specific precedence level.
/// Returns Some((meta, op, total_len)) for meta-ops,
/// or Some(("", op, total_len)) for plain bracket infix.
fn try_bracket_op_at_level(input: &str, level: &OpPrecedence) -> Option<(String, String, usize)> {
    // Try R[op], Z[op], X[op]
    // Skip Z/X meta-ops — they need list-infix comma handling
    if let Some((meta, op, len)) = parse_meta_op(input)
        && meta != "Z"
        && meta != "X"
    {
        let full_op = format!("{}{}", meta, op);
        if std::mem::discriminant(&classify_base_op(&full_op)) == std::mem::discriminant(level) {
            return Some((meta, op, len));
        }
    }
    // Try [op] bracket infix
    if let Some(bracket_infix) = parse_bracket_infix_op(input) {
        match &bracket_infix {
            BracketInfix::PlainOp(op, len) => {
                if std::mem::discriminant(&classify_base_op(op)) == std::mem::discriminant(level) {
                    return Some((String::new(), op.clone(), *len));
                }
            }
            BracketInfix::MetaOp(meta, op, len) if meta != "Z" && meta != "X" => {
                let full_op = format!("{}{}", meta, op);
                if std::mem::discriminant(&classify_base_op(&full_op))
                    == std::mem::discriminant(level)
                {
                    return Some((meta.clone(), op.clone(), *len));
                }
            }
            _ => {}
        }
    }
    None
}

/// Addition/subtraction: + -
pub(super) fn additive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = multiplicative_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_additive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after additive operator", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Custom infix ops at additive level exactly (is equiv<+>)
        {
            use super::super::stmt::simple::PREC_ADDITIVE;
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_ADDITIVE - 1,
                PREC_ADDITIVE,
                multiplicative_expr,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((_, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
            && matches!(classify_base_op(&op_name), OpPrecedence::Additive)
        {
            break;
        }
        // Try bracket meta-op at additive level: R[+], R[-], [R+], [R-], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Additive) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket additive operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                // Plain bracket infix: [+] or [-]
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Call `prefix_expr` and then apply whitespace-separated dotty method calls.
/// This places ws-dot at a precedence level between prefix/power and multiplicative,
/// so that `-2**2 . abs` parses as `(-(2**2)).abs` (ws-dot looser than prefix and **)
/// while `-1 * -1 . abs` parses as `-1 * ((-1).abs)` (ws-dot tighter than *).
fn prefix_expr_with_ws_dot(input: &str) -> PResult<'_, Expr> {
    let (rest, expr) = prefix_expr(input)?;
    postfix_expr_continue(rest, expr)
}

/// Multiplication/division: * / % div mod gcd lcm
pub(super) fn multiplicative_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = prefix_expr_with_ws_dot(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_multiplicative_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = prefix_expr_with_ws_dot(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after multiplicative operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Custom infix ops at multiplicative level (between additive and multiplicative inclusive)
        // (covers is equiv<*>, is tighter<+>, is looser<*>)
        {
            use super::super::stmt::simple::{PREC_ADDITIVE, PREC_MULTIPLICATIVE};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_ADDITIVE,
                PREC_MULTIPLICATIVE,
                prefix_expr_with_ws_dot,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((_, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
            && matches!(classify_base_op(&op_name), OpPrecedence::Multiplicative)
        {
            break;
        }
        // Try bracket meta-op at multiplicative level: R[*], R[/], [R*], [R/], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Multiplicative) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = prefix_expr_with_ws_dot(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket multiplicative operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Autoincrement prefix: ++expr, --expr
/// Binds tighter than ** (exponentiation) but looser than postfix.
/// e.g. `++$i ** 2` parses as `(++$i) ** 2`.
fn autoincrement_expr(
    input: &str,
    base_parser: fn(&str) -> PResult<'_, Expr>,
) -> PResult<'_, Expr> {
    use super::operators::PrefixUnaryOp;
    use super::operators::parse_prefix_unary_op;
    if let Some((op @ (PrefixUnaryOp::PreInc | PrefixUnaryOp::PreDec), len)) =
        parse_prefix_unary_op(input)
    {
        let rest = &input[len..];
        // Recurse to allow nested ++/-- (e.g. ++++$x)
        let (rest, expr) = autoincrement_expr(rest, base_parser)?;
        return Ok((
            rest,
            Expr::Unary {
                op: op.token_kind(),
                expr: Box::new(expr),
            },
        ));
    }
    base_parser(input)
}

/// Exponentiation: **
/// Uses tight postfix (no whitespace-separated dotty) so that ws-dot
/// method calls bind looser than `**` and prefix operators.
/// e.g. `-2**2 . abs` parses as `(-(2**2)).abs` = `4`.
pub(super) fn power_expr(input: &str) -> PResult<'_, Expr> {
    power_expr_inner(input, super::postfix::postfix_expr_tight_pub)
}

/// Like `power_expr` but uses tight postfix parsing (no whitespace-separated
/// dotty method calls).  Used as the operand parser for prefix `^` so that
/// `^2**64` correctly parses as `^(2**64)` while `^10 .batch(3)` still parses
/// as `(^10).batch(3)`.
pub(super) fn power_expr_tight(input: &str) -> PResult<'_, Expr> {
    power_expr_inner(input, super::postfix::postfix_expr_tight_pub)
}

fn power_expr_inner(input: &str, base_parser: fn(&str) -> PResult<'_, Expr>) -> PResult<'_, Expr> {
    let (mut rest, mut base) = autoincrement_expr(input, base_parser)?;
    // Check for custom infixes at power level (tighter than multiplicative)
    loop {
        let (r, _) = ws(rest)?;
        // Custom infix ops at power level (between multiplicative and prefix exclusive)
        // (covers is equiv<**>, is tighter<*>, is tighter<**>)
        {
            use super::super::stmt::simple::{PREC_MULTIPLICATIVE, PREC_PREFIX};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut base,
                PREC_MULTIPLICATIVE,
                PREC_PREFIX - 1,
                super::postfix::postfix_expr_tight_pub,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some(stripped) = r.strip_prefix("**") {
            let (r, _) = ws(stripped)?;
            let (r, exp) = super::postfix::prefix_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected exponent expression after '**'", r.len())
            })?; // right-associative, allow prefix on RHS
            base = Expr::Binary {
                left: Box::new(base),
                op: TokenKind::StarStar,
                right: Box::new(exp),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, base))
}

#[cfg(test)]
mod tests {
    use super::parse_meta_op;

    #[test]
    fn parse_meta_op_accepts_set_union_variants() {
        assert_eq!(
            parse_meta_op("Z(|) 2..4").map(|(m, op, len)| (m, op, len)),
            Some(("Z".to_string(), "(|)".to_string(), 4))
        );
        assert_eq!(
            parse_meta_op("Z∪ 2..4").map(|(m, op, len)| (m, op, len)),
            Some(("Z".to_string(), "∪".to_string(), 1 + "∪".len()))
        );
    }

    #[test]
    fn parse_meta_op_accepts_not_container_identity() {
        assert_eq!(
            parse_meta_op("X!=:= $a, $b").map(|(m, op, len)| (m, op, len)),
            Some(("X".to_string(), "!=:=".to_string(), 5))
        );
    }

    #[test]
    fn parse_meta_op_accepts_container_identity() {
        assert_eq!(
            parse_meta_op("X=:= $a, $b").map(|(m, op, len)| (m, op, len)),
            Some(("X".to_string(), "=:=".to_string(), 4))
        );
    }
}
