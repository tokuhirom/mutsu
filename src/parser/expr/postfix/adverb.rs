use crate::ast::{ExistsAdverb, Expr};
use crate::parser::expr::expression;
use crate::parser::helpers::{is_ident_char, ws};
use crate::parser::parse_result::parse_char;
use crate::symbol::Symbol;
use crate::value::Value;

/// Try to parse a secondary adverb after :exists/:!exists.
/// Returns (remaining_input, adverb).
pub(crate) fn parse_exists_secondary_adverb(input: &str) -> (&str, ExistsAdverb) {
    if input.starts_with(":!kv") && !is_ident_char(input.as_bytes().get(4).copied()) {
        return (&input[4..], ExistsAdverb::NotKv);
    }
    if input.starts_with(":kv") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::Kv);
    }
    if input.starts_with(":!p") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::NotP);
    }
    if input.starts_with(":p") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::P);
    }
    if input.starts_with(":!v") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::NotV);
    }
    if input.starts_with(":!k") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return (&input[3..], ExistsAdverb::InvalidNotK);
    }
    if input.starts_with(":k") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::InvalidK);
    }
    if input.starts_with(":v") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return (&input[2..], ExistsAdverb::InvalidV);
    }
    (input, ExistsAdverb::None)
}

/// Parse dynamic subscript adverb: :$delete, :$exists — the variable value
/// determines whether the adverb is active at runtime.
pub(crate) fn parse_dynamic_subscript_adverb(input: &str) -> Option<&str> {
    if !input.starts_with(":$") {
        return None;
    }
    let r = &input[2..];
    // Read identifier
    let end = r
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(r.len());
    if end == 0 {
        return None;
    }
    let name = &r[..end];
    // Only recognize known subscript adverb names
    match name {
        "delete" | "exists" => Some(&r[end..]),
        _ => None,
    }
}

/// Parse a subscript adverb like `:k`, `:!kv`, `:k($ok)`, `:kv(0)`, etc.
/// Returns (remaining_input, mode_string, optional_dynamic_expr).
/// When the adverb has a dynamic expression argument (e.g. `:k($var)`),
/// the third element is `Some(expr)` and the mode is the base adverb name.
pub(crate) fn parse_subscript_adverb_with_expr(
    input: &str,
) -> Option<(&str, &'static str, Option<Expr>)> {
    if input.starts_with(":!kv") && !is_ident_char(input.as_bytes().get(4).copied()) {
        return Some((&input[4..], "not-kv", None));
    }
    if input.starts_with(":!p") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-p", None));
    }
    if input.starts_with(":!k") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-k", None));
    }
    if input.starts_with(":!v") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "not-v", None));
    }
    if let Some(rest) = input.strip_prefix(":kv(0)") {
        return Some((rest, "kv0", None));
    }
    if let Some(rest) = input.strip_prefix(":kv(1)") {
        return Some((rest, "kv", None));
    }
    // :kv(expr) — dynamic adverb with expression
    if input.starts_with(":kv(")
        && let Some((rest, expr)) = try_parse_adverb_expr(&input[3..])
    {
        return Some((rest, "kv", Some(expr)));
    }
    if input.starts_with(":kv") && !is_ident_char(input.as_bytes().get(3).copied()) {
        return Some((&input[3..], "kv", None));
    }
    if let Some(rest) = input.strip_prefix(":p(0)") {
        return Some((rest, "p0", None));
    }
    if let Some(rest) = input.strip_prefix(":p(1)") {
        return Some((rest, "p", None));
    }
    // :p(expr)
    if input.starts_with(":p(")
        && let Some((rest, expr)) = try_parse_adverb_expr(&input[2..])
    {
        return Some((rest, "p", Some(expr)));
    }
    if input.starts_with(":p") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "p", None));
    }
    if let Some(rest) = input.strip_prefix(":k(0)") {
        return Some((rest, "k0", None));
    }
    if let Some(rest) = input.strip_prefix(":k(1)") {
        return Some((rest, "k", None));
    }
    // :k(expr)
    if input.starts_with(":k(")
        && let Some((rest, expr)) = try_parse_adverb_expr(&input[2..])
    {
        return Some((rest, "k", Some(expr)));
    }
    if input.starts_with(":k") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "k", None));
    }
    if let Some(rest) = input.strip_prefix(":v(0)") {
        return Some((rest, "v0", None));
    }
    if let Some(rest) = input.strip_prefix(":v(1)") {
        return Some((rest, "v", None));
    }
    // :v(expr)
    if input.starts_with(":v(")
        && let Some((rest, expr)) = try_parse_adverb_expr(&input[2..])
    {
        return Some((rest, "v", Some(expr)));
    }
    if input.starts_with(":v") && !is_ident_char(input.as_bytes().get(2).copied()) {
        return Some((&input[2..], "v", None));
    }
    None
}

/// Try to parse a parenthesized expression for a subscript adverb argument.
/// Input starts at `(`. Returns `(remaining_after_close_paren, expr)`.
pub(crate) fn try_parse_adverb_expr(input: &str) -> Option<(&str, Expr)> {
    let r = input.strip_prefix('(')?;
    let (r, _) = ws(r).ok()?;
    let (r, expr) = expression(r).ok()?;
    let (r, _) = ws(r).ok()?;
    let (r, _) = parse_char(r, ')').ok()?;
    Some((r, expr))
}

/// Try to parse an unknown `:identifier` adverb (not k/v/kv/p/exists/delete).
pub(crate) fn try_parse_unknown_adverb(input: &str) -> Option<(&str, String)> {
    if !input.starts_with(':') {
        return None;
    }
    let rest = &input[1..];
    let rest = rest.strip_prefix('!').unwrap_or(rest);
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_alphabetic() {
        return None;
    }
    let end = rest
        .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(rest.len());
    if end == 0 {
        return None;
    }
    let name = &rest[..end];
    if matches!(name, "k" | "v" | "kv" | "p" | "exists" | "delete") {
        return None;
    }
    let after = &rest[end..];
    let final_rest = if after.starts_with('(') {
        after.find(')').map_or(after, |close| &after[close + 1..])
    } else {
        after
    };
    Some((final_rest, name.to_string()))
}

/// Determine "element access" vs "slice" from the target and index.
/// Hash access is always "slice"; array single-element is "element access".
pub(crate) fn determine_subscript_what(target: &Expr, index_expr: &Expr) -> String {
    // A *zen* slice (`@a[]` / `%h{}`, modelled as a `Literal(Whatever)` index by
    // the empty-subscript-with-adverb path) reports "zen slice".
    if matches!(index_expr, Expr::Literal(Value::Whatever)) {
        return "zen slice".to_string();
    }
    // A whatever slice (`@a[*]`, parsed as a bare Whatever index) reports
    // "whatever slice". A hash zen slice keeps the bracket-kind descriptor
    // (`{} slice`), which the runtime downgrades to plain "slice" on a nogo
    // conflict (see `builtin_subscript_adverb_error`).
    if matches!(index_expr, Expr::Whatever) {
        return if matches!(target, Expr::HashVar(_)) {
            "{} slice".to_string()
        } else {
            "whatever slice".to_string()
        };
    }
    if matches!(target, Expr::HashVar(_)) {
        return "slice".to_string();
    }
    // A Range subscript (`@a[1..2]`) is a multi-element slice.
    if let Expr::Binary { op, .. } = index_expr
        && matches!(
            op,
            crate::token_kind::TokenKind::DotDot
                | crate::token_kind::TokenKind::DotDotCaret
                | crate::token_kind::TokenKind::CaretDotDot
                | crate::token_kind::TokenKind::CaretDotDotCaret
        )
    {
        return "slice".to_string();
    }
    match index_expr {
        Expr::ArrayLiteral(items) if items.len() != 1 => "slice".to_string(),
        Expr::ArrayVar(_) => "slice".to_string(),
        _ => "element access".to_string(),
    }
}

/// Normalize an adverb name (strip "not-" prefix and "0" suffix).
pub(crate) fn normalize_adverb_name(s: &str) -> String {
    let s = s.strip_prefix("not-").unwrap_or(s);
    s.strip_suffix('0').unwrap_or(s).to_string()
}

/// Build a `__mutsu_subscript_adverb_error` call for X::Adverb.
pub(crate) fn build_adverb_error_call(
    what: &str,
    source: &str,
    nogo: &[String],
    unexpected: &[String],
) -> Expr {
    let mut args = vec![
        Expr::Literal(Value::str(what.to_string())),
        Expr::Literal(Value::str(source.to_string())),
    ];
    for a in nogo {
        args.push(Expr::Literal(Value::str(format!("__nogo__{}", a))));
    }
    for u in unexpected {
        args.push(Expr::Literal(Value::str(format!("__unexpected__{}", u))));
    }
    Expr::Call {
        name: Symbol::intern("__mutsu_subscript_adverb_error"),
        args,
    }
}

/// Consume all remaining adverbs (known and unknown) after a subscript.
pub(crate) fn collect_remaining_adverbs<'a>(
    start: &'a str,
    known: &mut Vec<String>,
    unknown: &mut Vec<String>,
) -> &'a str {
    let mut r = start;
    loop {
        let r2 = ws(r).map_or(r, |(r2, _)| r2);
        if let Some((r3, next_adv, _)) = parse_subscript_adverb_with_expr(r2) {
            known.push(normalize_adverb_name(next_adv));
            r = r3;
        } else if let Some((r3, unk_name)) = try_parse_unknown_adverb(r2) {
            unknown.push(unk_name);
            r = r3;
        } else {
            break;
        }
    }
    r
}

/// Extract the variable name from a MultiDimIndex target expression.
pub(crate) fn multidim_target_var_name(target: &Expr) -> String {
    match target {
        Expr::ArrayVar(name) => format!("@{}", name),
        Expr::HashVar(name) => format!("%{}", name),
        Expr::Var(name) => name.clone(),
        _ => String::new(),
    }
}

pub(crate) enum DeleteAdverb {
    NoDelete,
    Delete(Option<Expr>),
}

pub(crate) fn parse_delete_adverb(input: &str) -> Option<(&str, DeleteAdverb)> {
    if input.starts_with(":!delete") && !is_ident_char(input.as_bytes().get(8).copied()) {
        return Some((&input[8..], DeleteAdverb::NoDelete));
    }
    if input.starts_with(":delete") && !is_ident_char(input.as_bytes().get(7).copied()) {
        let mut r = &input[7..];
        if let Some(r_stripped) = r.strip_prefix('(')
            && let Ok((r2, _)) = ws(r_stripped)
            && let Ok((r2, cond)) = expression(r2)
            && let Ok((r2, _)) = ws(r2)
            && let Ok((r2, _)) = parse_char(r2, ')')
        {
            return Some((r2, DeleteAdverb::Delete(Some(cond))));
        }
        // :delete with no argument, or malformed parens (leave for outer parser).
        if r.starts_with('(') {
            return None;
        }
        r = &input[7..];
        return Some((r, DeleteAdverb::Delete(None)));
    }
    None
}

pub(crate) fn subscript_adverb_expr_with_cond(
    expr: Expr,
    adverb: &'static str,
    cond: Option<Expr>,
) -> Expr {
    // Handle MultiDimIndex: @a[0;0;0]:kv etc.
    if let Expr::MultiDimIndex { target, dimensions } = expr {
        let mut args = vec![*target, Expr::Literal(Value::str(adverb.to_string()))];
        args.extend(dimensions);
        if let Some(cond_expr) = cond {
            args.push(Expr::Literal(Value::str("__adverb_cond__".to_string())));
            args.push(cond_expr);
        }
        return Expr::Call {
            name: Symbol::intern("__mutsu_multidim_subscript_adverb"),
            args,
        };
    }
    let Expr::Index { target, index, .. } = expr else {
        return expr;
    };
    let var_name = match target.as_ref() {
        Expr::ArrayVar(name) => Expr::Literal(Value::str(format!("@{}", name))),
        Expr::HashVar(name) => Expr::Literal(Value::str(format!("%{}", name))),
        _ => Expr::Literal(Value::Nil),
    };
    let mut args = vec![
        *target,
        *index,
        Expr::Literal(Value::str(adverb.to_string())),
        var_name,
    ];
    // When a dynamic condition is provided (e.g., `:k($ok)`), pass it as
    // a named Pair so the runtime can decide keep_missing at evaluation time.
    if let Some(cond_expr) = cond {
        args.push(Expr::Literal(Value::str("__adverb_cond__".to_string())));
        args.push(cond_expr);
    }
    Expr::Call {
        name: Symbol::intern("__mutsu_subscript_adverb"),
        args,
    }
}

/// Try to parse :exists or :!exists adverb on a subscript expression.
/// Returns (remaining_input, exists_expr) or None if no adverb found.
pub(crate) fn try_parse_exists_adverb(input: &str, target: Expr) -> Option<(&str, Expr)> {
    let r = input;
    let (r, negated) = if r.starts_with(":!exists") && !is_ident_char(r.as_bytes().get(8).copied())
    {
        (&r[8..], true)
    } else if r.starts_with(":exists") && !is_ident_char(r.as_bytes().get(7).copied()) {
        (&r[7..], false)
    } else {
        return None;
    };
    // Check for parameterized argument: :exists(expr)
    let (r, arg) = if let Some(r_stripped) = r.strip_prefix('(') {
        let r2 = r_stripped;
        if let Ok((r2, _)) = ws(r2) {
            if let Ok((r2, arg_expr)) = expression(r2) {
                if let Ok((r2, _)) = ws(r2) {
                    if let Ok((r2, _)) = parse_char(r2, ')') {
                        (r2, Some(Box::new(arg_expr)))
                    } else {
                        (r, None)
                    }
                } else {
                    (r, None)
                }
            } else {
                (r, None)
            }
        } else {
            (r, None)
        }
    } else {
        (r, None)
    };
    // Check for secondary adverb
    let (r, adverb) = parse_exists_secondary_adverb(r);
    Some((
        r,
        Expr::Exists {
            target: Box::new(target),
            negated,
            delete: false,
            arg,
            adverb,
        },
    ))
}

pub(crate) fn supports_postfix_call_adverbs(expr: &Expr) -> bool {
    match expr {
        Expr::Call { name, .. } => {
            let n = name.resolve();
            !n.starts_with("__mutsu_subscript_adverb") && !n.starts_with("__mutsu_multidim")
        }
        Expr::MethodCall { .. }
        | Expr::CallOn { .. }
        | Expr::HyperMethodCall { .. }
        | Expr::HyperMethodCallDynamic { .. } => true,
        _ => false,
    }
}
