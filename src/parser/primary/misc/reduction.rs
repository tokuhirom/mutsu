use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::{skip_balanced_parens, ws};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::primary::parse_call_arg_list;
use crate::parser::primary::var::parse_ident_with_hyphens;
use crate::symbol::Symbol;

/// Known reduction operators (must be listed to distinguish from array literals).
const REDUCTION_OPS: &[&str] = &[
    "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^", "+<", "+>",
    "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=", ">=", "<=>", "===",
    "=:=", "!=:=", "=>", "=", "eqv", "eq", "ne", "lt", "gt", "le", "ge", "leg", "cmp", "coll",
    "unicmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", "andthen", "orelse", "xor",
    ",", "after", "before", "X", "Z", "x", "xx", "&", "|", "^", "o", "∘", "⊍", "div", "mod",
    "minmax",
];

/// Find the matching `]` for a `[` at position 0, respecting nesting.
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
/// e.g., "+" → "+", "R-" → "R-", "R[+]" → "R+", "[+]" → "+", "R[R[R-]]" → "RRR-"
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

/// Check if the given op (after flattening) is a valid reduction operator.
/// Handles R/Z/X meta-prefix chains by stripping them to find the base op.
fn is_valid_reduction_op(op: &str) -> bool {
    let mut s = op;
    // Strip meta prefixes while keeping bare operators like `X` intact.
    while s.len() > 1 {
        if let Some(rest) = s.strip_prefix('R') {
            s = rest;
        } else if let Some(rest) = s.strip_prefix('Z') {
            s = rest;
        } else if let Some(rest) = s.strip_prefix('X') {
            s = rest;
        } else {
            break;
        }
    }
    // Also support scan form (\op) and negated operators (!after),
    // while keeping operators that genuinely start with '!' (e.g. '!=').
    if let Some(after_scan) = s.strip_prefix('\\') {
        return is_valid_reduction_op(after_scan);
    }
    let s = if s == "∘" { "o" } else { s };
    if REDUCTION_OPS.contains(&s) {
        return true;
    }
    // Set operators in parenthesized and unicode form
    if matches!(
        s,
        "(-)"
            | "(|)"
            | "(&)"
            | "(.)"
            | "(^)"
            | "(elem)"
            | "(cont)"
            | "∪"
            | "∩"
            | "⊍"
            | "∖"
            | "⊖"
            | "∈"
            | "∋"
            | "(<=)"
            | "(>=)"
            | "(<)"
            | "(>)"
            | "⊆"
            | "⊇"
            | "⊂"
            | "⊃"
            | "⊄"
            | "⊅"
    ) {
        return true;
    }
    if let Some(stripped) = s.strip_prefix('!')
        && REDUCTION_OPS.contains(&stripped)
    {
        return true;
    }
    // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>, and Unicode variants
    if let Some(inner) = strip_hyper_delimiters(s) {
        return is_valid_reduction_op(inner);
    }
    is_custom_reduction_op(s)
}

/// Strip hyper operator delimiters (>>...<<, >>...>>, <<...<<, <<...>>)
/// and their Unicode variants, returning the inner operator if found.
fn strip_hyper_delimiters(s: &str) -> Option<&str> {
    let after_left = s
        .strip_prefix(">>")
        .or_else(|| s.strip_prefix("<<"))
        .or_else(|| s.strip_prefix('\u{00BB}'))
        .or_else(|| s.strip_prefix('\u{00AB}'))?;
    let inner = after_left
        .strip_suffix(">>")
        .or_else(|| after_left.strip_suffix("<<"))
        .or_else(|| after_left.strip_suffix('\u{00BB}'))
        .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
    if inner.is_empty() {
        return None;
    }
    Some(inner)
}

fn is_custom_reduction_op(op: &str) -> bool {
    if let Some(name) = op.strip_prefix('&') {
        return is_callable_reduction_name(name);
    }
    // Check if it's a user-declared infix symbol operator (e.g. T+, ⋅)
    if let Some((symbol, len)) =
        crate::parser::stmt::simple::match_user_declared_infix_symbol_op(op)
    {
        if len == op.len() {
            return true;
        }
        // Also accept the symbol as a prefix of op (shouldn't happen for reduction, but be safe)
        let _ = symbol;
    }
    // A bareword that is not a declared infix operator: accept it as a custom
    // reduction op ONLY if it looks like an operator name (lowercase/`_`-initial).
    // An uppercase-initial identifier is a type name or class (`[Any]`, `[Int]`,
    // `[Exception]`), so `[Type]` must parse as an array literal, not a reduction
    // (`[Any].raku` is `[Any]` array literal's `.raku`, NOT a reduction with op
    // "Any"). User-declared uppercase infixes are already matched above by
    // `match_user_declared_infix_symbol_op`.
    if !op
        .chars()
        .next()
        .is_some_and(|c| c.is_lowercase() || c == '_')
    {
        return false;
    }
    if let Ok((rest, _)) = parse_ident_with_hyphens(op) {
        return rest.is_empty();
    }
    false
}

fn is_callable_reduction_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    if (name.starts_with("infix:<")
        || name.starts_with("prefix:<")
        || name.starts_with("postfix:<")
        || name.starts_with("term:<"))
        && name.ends_with('>')
    {
        return true;
    }
    if let Ok((rest, _)) = parse_ident_with_hyphens(name) {
        return rest.is_empty();
    }
    false
}

/// Parse a reduction operator: [+], [*], [~], [min], [[+]], [R[+]], etc.
pub(crate) fn reduction_op(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    // Use bracket-matching to find the closing ] (supports nesting)
    let end =
        find_matching_bracket(input).ok_or_else(|| PError::expected("']' closing reduction"))?;
    let inner = &input[1..end];
    if inner.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    // Flatten nested bracket notation: [[+]] → "+", [R[+]] → "R+"
    let op = match flatten_bracket_op(inner).as_str() {
        "∘" => "o".to_string(),
        other => other.to_string(),
    };
    // A `&callable` reduction operator requires the double-bracket form
    // `[[&foo]]` (the inner `[&foo]` is itself the bracketed infix). A single
    // `[&foo]` is an array literal containing the sub, NOT a reduction — in
    // Raku `[&foo]` alone is a term (array literal), `2 [&foo] 3` is an infix,
    // and only `[[&foo]] @a` is the reduction. Both forms flatten to op
    // `&foo`, so distinguish by whether the raw inner was itself bracketed.
    let meta = |c: char| c == 'R' || c == 'Z' || c == 'X' || c == '\\';
    let is_callable_amp = |s: &str| {
        s.strip_prefix('&')
            .and_then(|rest| rest.chars().next())
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    };
    if is_callable_amp(op.trim_start_matches(meta))
        && !inner.trim_start().trim_start_matches(meta).starts_with('[')
    {
        return Err(PError::expected(
            "callable reduction requires double-bracket form [[&op]]",
        ));
    }
    // Only accept known operators (after flattening) to avoid confusion with array literals.
    if !is_valid_reduction_op(&op) {
        return Err(PError::expected("known reduction operator"));
    }
    // Non-associative "structural" infix operators cannot be used in reduction form.
    // They return Order, not Bool, and are diffy (not chaining).
    let base_for_check = op.strip_prefix('\\').unwrap_or(op.as_str());
    let base_for_check = base_for_check
        .strip_prefix('R')
        .filter(|s| !s.is_empty())
        .unwrap_or(base_for_check);
    if matches!(base_for_check, "leg" | "<=>" | "cmp" | "coll" | "unicmp") {
        return Err(PError::fatal(format!(
            "X::Syntax::CannotMeta: Cannot reduce with {base_for_check} because structural infix operators are diffy and not chaining"
        )));
    }
    let r = &input[end + 1..];
    let call_style_operand = r.starts_with('(');
    // Zero-argument reduction: [op] with no operands → identity element
    // Also accept comma-separated context when the operator is clearly a symbolic op
    // (not an identifier that could be an array element like [Exception]).
    let is_symbol_op = !op
        .chars()
        .next()
        .is_some_and(|c| c.is_alphabetic() || c == '_');
    if r.is_empty()
        || r.starts_with(';')
        || r.starts_with('}')
        || r.starts_with(')')
        || (r.starts_with(',') && is_symbol_op)
    {
        return Ok((
            r,
            Expr::Reduction {
                op,
                expr: Box::new(Expr::ArrayLiteral(vec![])),
            },
        ));
    }
    // Call-style: [op](args) — use parse_call_arg_list to handle semicolons
    if call_style_operand {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, args) = crate::parser::primary::regex::parse_call_arg_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let expr = if args.len() == 1 {
            args.into_iter().next().unwrap()
        } else {
            Expr::ArrayLiteral(args)
        };
        return Ok((
            r,
            Expr::Reduction {
                op,
                expr: Box::new(expr),
            },
        ));
    }
    // Listop-style reduction (`[+] @a`) requires whitespace between `]`
    // and the operand for symbolic operators. `[+]@a` would be two terms
    // in a row. Alphabetic operators like `[min]` are ambiguous with array
    // literals like `[Exception]` and are not enforced here.
    if is_symbol_op
        && !(r.starts_with(' ')
            || r.starts_with('\t')
            || r.starts_with('\n')
            || r.starts_with('\r')
            || r.starts_with('#'))
    {
        return Err(PError::fatal(
            "X::Syntax::Confused: Two terms in a row".to_string(),
        ));
    }
    let (r, _) = ws(r)?;
    // Parse comma-separated list as the operand
    let (r, first) = parse_reduction_operand(r)?;
    let mut items = vec![first];
    let mut rest = r;
    if true {
        loop {
            let (r, _) = ws_inner(rest);
            if !r.starts_with(',') {
                break;
            }
            let r = &r[1..];
            let (r, _) = ws_inner(r);
            // Stop at end-of-input, semicolon, closing brackets, or statement modifiers
            if r.is_empty()
                || r.starts_with(';')
                || r.starts_with('}')
                || r.starts_with(')')
                || r.starts_with(']')
            {
                rest = r;
                break;
            }
            if let Ok((r, next)) = parse_reduction_operand(r) {
                items.push(next);
                rest = r;
            } else {
                return Err(PError::expected_at(
                    "expression after ',' in reduction list",
                    r,
                ));
            }
        }
    }
    let mut items = merge_list_infix_seeds(items);
    items = merge_sequence_seeds(items);
    let expr = if items.len() == 1 {
        items.remove(0)
    } else {
        Expr::ArrayLiteral(items)
    };
    Ok((
        rest,
        Expr::Reduction {
            op,
            expr: Box::new(expr),
        },
    ))
}

fn parse_reduction_operand(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if let Some(r) = r.strip_prefix('|') {
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        return Ok((
            r,
            Expr::Call {
                name: Symbol::intern("slip"),
                args: vec![expr],
            },
        ));
    }
    expression(r)
}

fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(
            op,
            crate::token_kind::TokenKind::DotDotDot | crate::token_kind::TokenKind::DotDotDotCaret
        )
    {
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

fn merge_list_infix_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::MetaOp {
        meta,
        op,
        left,
        right,
    } = last
        && (meta == "X" || meta == "Z")
    {
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(Expr::ArrayLiteral(seeds)),
            right: right.clone(),
        };
        return vec![merged];
    }
    items
}

pub(crate) fn reduction_call_style_expr(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    let end =
        find_matching_bracket(input).ok_or_else(|| PError::expected("']' closing reduction"))?;
    let inner = &input[1..end];
    if inner.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    let op = match flatten_bracket_op(inner).as_str() {
        "∘" => "o".to_string(),
        other => other.to_string(),
    };
    if !is_valid_reduction_op(&op) {
        return Err(PError::expected("known reduction operator"));
    }
    let r = &input[end + 1..];
    // Call-style reduction requires `(` immediately after `]` (no whitespace).
    // `[Z+](args)` is call-style; `[Z+] (a, b), (c, d)` is listop-style
    // and should be handled by `reduction_op` instead.
    if !r.starts_with('(') {
        return Err(PError::expected("call-style reduction operand"));
    }
    let rest_after_operand = skip_balanced_parens(r);
    if rest_after_operand == r {
        return Err(PError::expected("')' closing reduction argument"));
    }
    let close_pos = r.len() - rest_after_operand.len();
    if close_pos <= 1 {
        return Err(PError::expected("expression in call-style reduction"));
    }
    let inner = &r[1..close_pos - 1];
    let (inner_rest, args) = parse_call_arg_list(inner)?;
    let (inner_rest, _) = ws(inner_rest)?;
    if !inner_rest.is_empty() {
        return Err(PError::expected("')' in call-style reduction"));
    }
    if args.is_empty() {
        return Err(PError::expected("expression in call-style reduction"));
    }
    let inner_expr = if args.len() == 1 {
        args.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(args)
    };
    Ok((
        rest_after_operand,
        Expr::Reduction {
            op,
            expr: Box::new(inner_expr),
        },
    ))
}
