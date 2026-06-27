use crate::parser::helpers::is_ident_char;
use crate::parser::stmt::simple::match_user_declared_infix_symbol_op;
use crate::token_kind::TokenKind;

/// Raku's "statement-ending block" rule: a closing brace `}` followed by a
/// newline terminates the current statement, so an infix operator on the next
/// line must NOT be consumed as part of the same expression.
///
/// `input` is the string that was passed to the current precedence function.
/// `rest` is the remaining input right after the left-hand expression.
/// `after_ws` is the remaining input after consuming whitespace from `rest`.
///
/// Returns `true` when the infix loop should break.
pub(crate) fn block_newline_terminates(input: &str, rest: &str, after_ws: &str) -> bool {
    let consumed = input.len() - rest.len();
    if consumed == 0 {
        return false;
    }
    // Check if the character just before `rest` is `}`
    if input.as_bytes()[consumed - 1] != b'}' {
        return false;
    }
    // Check if the whitespace gap between rest and after_ws contains a newline
    let gap = &rest[..rest.len() - after_ws.len()];
    gap.contains('\n')
}

pub(crate) fn parse_infix_func_op(input: &str) -> Option<(Option<String>, String, usize)> {
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
pub(crate) fn op_str_to_token_kind(op: &str) -> Option<TokenKind> {
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
        "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "leg" | "cmp" | "coll" | "unicmp" | "min"
        | "max" | "gcd" | "lcm" | "and" | "or" | "not" | "after" | "before" => {
            Some(TokenKind::Ident(op.to_string()))
        }
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
    "+^", "?&", "?|", "?^", "cmp", "coll", "unicmp", "min", "max", "eq", "ne", "lt", "gt", "le",
    "ge", "leg", "and", "or", "not", "after", "before", "gcd", "lcm", ",", "(|)", "(&)", "(.)",
    "(+)", "(^)", "(elem)", "(cont)", "∪", "∩", "⊍", "⊎", "⊖", "∈", "∋",
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
pub(crate) fn parse_meta_op(input: &str) -> Option<(String, String, usize)> {
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
        // Sequence (`...`/`...^`) and range (`..`/`..^`/`^..`/`^..^`) operators must
        // precede the shorter forms: `...` before `..`, `^..^` before `^..`, `..^`
        // before `..`. This lets `R..`/`R^..^`/etc. (reversed range) parse as a
        // meta-op over the range base op.
        "...^", "...", "…^", "…", "^..^", "^..", "..^", "..", "**", "=>", "==", "!=:=", "=:=", "!=",
        "<=", ">=", "~~", "%%", "//", "&&", "||", "+&", "+|", "+^", "+<", "+>", "~&", "~|", "~^",
        "~", "+", "-", "*", "/", "%", "<", ">", ",",
    ];
    for op in ops {
        if r.starts_with(op) {
            return Some((meta.to_string(), op.to_string(), 1 + op.len()));
        }
    }
    // Try word operators: cmp, min, max, eq, ne, lt, gt, le, ge, leg
    let word_ops: &[&str] = &[
        "unicmp", "cmp", "coll", "min", "max", "eq", "ne", "lt", "gt", "le", "ge", "leg",
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
    let user_sym = match_user_declared_infix_symbol_op(r);
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

pub(crate) fn strip_sequence_op(input: &str) -> Option<(&str, TokenKind, &str)> {
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
pub(crate) enum BracketInfix {
    /// A plain operator like `+`, `-`, `*`, `cmp`
    PlainOp(String, usize),
    /// A meta operator like `R-`, `Z*`, `Zcmp`
    MetaOp(String, String, usize),
    /// A user-defined infix like `blue`
    UserInfix(String, usize),
}

pub(crate) fn parse_bracket_infix_op(input: &str) -> Option<BracketInfix> {
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
    if let Some((symbol, len)) = match_user_declared_infix_symbol_op(inner)
        && len == inner.len()
    {
        return Some(BracketInfix::UserInfix(symbol, total_len));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::parse_meta_op;

    #[test]
    fn parse_meta_op_accepts_set_union_variants() {
        assert_eq!(
            parse_meta_op("Z(|) 2..4"),
            Some(("Z".to_string(), "(|)".to_string(), 4))
        );
        assert_eq!(
            parse_meta_op("Z∪ 2..4"),
            Some(("Z".to_string(), "∪".to_string(), 1 + "∪".len()))
        );
    }

    #[test]
    fn parse_meta_op_accepts_not_container_identity() {
        assert_eq!(
            parse_meta_op("X!=:= $a, $b"),
            Some(("X".to_string(), "!=:=".to_string(), 5))
        );
    }

    #[test]
    fn parse_meta_op_accepts_container_identity() {
        assert_eq!(
            parse_meta_op("X=:= $a, $b"),
            Some(("X".to_string(), "=:=".to_string(), 4))
        );
    }
}
