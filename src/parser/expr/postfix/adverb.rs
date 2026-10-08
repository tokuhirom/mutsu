use super::named_adverb::has_subscript_named_adverb;
pub(crate) use crate::ast::subscript_adverb::{
    build_adverb_error_call, multidim_target_var_name, subscript_adverb_expr_with_cond,
};
use crate::ast::subscript_adverb::{conditional_delete, deleting, exists_node};
use crate::ast::{ExistsAdverb, Expr};
use crate::parser::expr::expression;
use crate::parser::helpers::{is_ident_char, ws};
use crate::parser::parse_result::parse_char;

/// Try to parse a secondary adverb after :exists/:!exists.
/// Returns (remaining_input, adverb).
pub(crate) fn parse_exists_secondary_adverb(input: &str) -> (&str, ExistsAdverb) {
    if let Some((canonical, negated, rest)) =
        crate::parser::stmt::simple::l10n_match_adverb("adverb-pc", input)
    {
        let adverb = match (canonical.as_str(), negated) {
            ("kv", false) => ExistsAdverb::Kv,
            ("kv", true) => ExistsAdverb::NotKv,
            ("p", false) => ExistsAdverb::P,
            ("p", true) => ExistsAdverb::NotP,
            ("v", false) => ExistsAdverb::InvalidV,
            ("v", true) => ExistsAdverb::NotV,
            ("k", false) => ExistsAdverb::InvalidK,
            ("k", true) => ExistsAdverb::InvalidNotK,
            _ => return (input, ExistsAdverb::None),
        };
        return (rest, adverb);
    }
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
    let name = crate::parser::stmt::simple::l10n_adverb_alias("adverb-pc", name)
        .unwrap_or_else(|| name.to_string());
    // Only recognize known subscript adverb names
    match name.as_str() {
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
    if has_subscript_named_adverb(input) {
        return None;
    }
    // `:$k` / `:$v` / `:$kv` / `:$p`: the variable's value is the adverb's
    // runtime flag, exactly as `:k($k)`.
    if let Some(r) = input.strip_prefix(":$") {
        let end = r
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(r.len());
        let mode = match &r[..end] {
            "k" => Some("k"),
            "v" => Some("v"),
            "kv" => Some("kv"),
            "p" => Some("p"),
            _ => None,
        };
        if let Some(mode) = mode {
            return Some((&r[end..], mode, Some(Expr::Var(mode.to_string()))));
        }
    }
    if let Some((canonical, negated, rest)) =
        crate::parser::stmt::simple::l10n_match_adverb("adverb-pc", input)
    {
        let mode = match (canonical.as_str(), negated) {
            ("kv", true) => "not-kv",
            ("kv", false) => "kv",
            ("p", true) => "not-p",
            ("p", false) => "p",
            ("k", true) => "not-k",
            ("k", false) => "k",
            ("v", true) => "not-v",
            ("v", false) => "v",
            _ => return None,
        };
        if let Some(rest) = rest.strip_prefix("(0)") {
            return Some((
                rest,
                match mode {
                    "kv" => "kv0",
                    "p" => "p0",
                    "k" => "k0",
                    "v" => "v0",
                    _ => mode,
                },
                None,
            ));
        }
        if let Some(rest) = rest.strip_prefix("(1)") {
            return Some((rest, mode, None));
        }
        if !negated
            && rest.starts_with('(')
            && let Some((after, expr)) = try_parse_adverb_expr(rest)
        {
            return Some((after, mode, Some(expr)));
        }
        if rest.starts_with('(') {
            return None;
        }
        return Some((rest, mode, None));
    }
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

/// Determine "element access" vs "slice" from the target and index.
/// Hash access is always "slice"; array single-element is "element access".
pub(crate) fn determine_subscript_what(target: &Expr, index_expr: &Expr) -> String {
    // A *zen* slice (`@a[]` / `%h{}`, modelled as a `Literal(Whatever)` index by
    // the empty-subscript-with-adverb path) reports "zen slice".
    if matches!(index_expr, Expr::Literal(lit) if matches!(lit.view(), crate::value::ValueView::Whatever))
    {
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

/// Consume all remaining built-in adverbs after a subscript. (A chain with a
/// non-built-in adverb never gets here: it is lowered whole by
/// `named_adverb::lower_subscript_named_adverbs`.)
pub(crate) fn collect_remaining_adverbs<'a>(start: &'a str, known: &mut Vec<String>) -> &'a str {
    let mut r = start;
    loop {
        let r2 = ws(r).map_or(r, |(r2, _)| r2);
        if let Some((r3, next_adv, _)) = parse_subscript_adverb_with_expr(r2) {
            known.push(normalize_adverb_name(next_adv));
            r = r3;
        } else {
            break;
        }
    }
    r
}

/// The `:delete` lowering target for a multi-dim subscript.
///
/// `postcircumfix:<{; }>` has exactly two candidates -- `(\SELF, @indices)` and
/// `(\SELF, @indices, :$exists!)` -- so `:delete` on an ASSOCIATIVE multi-dim
/// subscript does not resolve at all, and rakudo throws `X::Multi::NoMatch`.
/// The positional `postcircumfix:<[; ]>` spelling accepts it.
///
/// Decided from the subscript FORM the source used rather than from the
/// receiver's runtime type: `$h{"a";"b"}` names a scalar whose value the delete
/// builtin cannot always resolve by name, and the form is what rakudo
/// dispatches on anyway.
///
/// `ndims == 1` is NOT the `{; }` form at all: a one-dimension multi-dim
/// subscript is only ever produced by the `||` splat (`%h{|| @indices}`), which
/// is an ordinary `postcircumfix:<{ }>` SLICE and does accept `:delete` --
/// rakudo answers `(42, 666)` for `t/multidim-splat-lazy.t`'s spelling.
///
/// And it is a 6.d-and-earlier rule, exactly like the multislice wrapper it
/// belongs to (`Interpreter::assoc_multislice`): **6.e grew the candidate** and
/// `%h{"a";"b";"c"}:delete` answers `42` there. Measured on rakudo v2026.06,
/// which is why `t/hash-multislice-container.t` (`use v6.e.PREVIEW`) keeps
/// asserting the deletion while the 6.d spelling now throws.
pub(crate) fn multidim_delete_fn(is_positional: bool, ndims: usize) -> &'static str {
    crate::ast::subscript_adverb::multidim_delete_fn(
        is_positional,
        ndims,
        crate::parser::current_language_version().starts_with("6.e"),
    )
}

pub(crate) enum DeleteAdverb {
    NoDelete,
    Delete(Option<Expr>),
}

/// A `:delete` written next to `:exists`, in either order, applied to the
/// `:exists` node: `:delete(COND)` deletes only when `COND` holds.
pub(crate) fn delete_with_exists(delete_adv: DeleteAdverb, exists_expr: Expr) -> Expr {
    apply_delete_adverb(delete_adv, exists_expr)
}

/// A `:delete` applied to a single-dimension subscript's read (the subscript,
/// its `:exists` node or its value-adverb call): `:delete(COND)` deletes only
/// when `COND` holds, `:!delete` leaves the read alone.
pub(crate) fn apply_delete_adverb(delete_adv: DeleteAdverb, read: Expr) -> Expr {
    match delete_adv {
        DeleteAdverb::NoDelete => read,
        DeleteAdverb::Delete(None) => deleting(&read),
        DeleteAdverb::Delete(Some(cond)) => conditional_delete(cond, deleting(&read), read),
    }
}

pub(crate) fn parse_delete_adverb(input: &str) -> Option<(&str, DeleteAdverb)> {
    if has_subscript_named_adverb(input) {
        return None;
    }
    if let Some((canonical, negated, rest)) =
        crate::parser::stmt::simple::l10n_match_adverb("adverb-pc", input)
        && canonical == "delete"
    {
        if negated {
            return Some((rest, DeleteAdverb::NoDelete));
        }
        if let Some(r_stripped) = rest.strip_prefix('(')
            && let Ok((r2, _)) = ws(r_stripped)
            && let Ok((r2, cond)) = expression(r2)
            && let Ok((r2, _)) = ws(r2)
            && let Ok((r2, _)) = parse_char(r2, ')')
        {
            return Some((r2, DeleteAdverb::Delete(Some(cond))));
        }
        if rest.starts_with('(') {
            return None;
        }
        return Some((rest, DeleteAdverb::Delete(None)));
    }
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

/// Try to parse :exists or :!exists adverb on a subscript expression.
/// Returns (remaining_input, exists_expr) or None if no adverb found.
pub(crate) fn try_parse_exists_adverb(input: &str, target: Expr) -> Option<(&str, Expr)> {
    if has_subscript_named_adverb(input) {
        return None;
    }
    let r = input;
    let (r, negated) = if let Some((canonical, negated, rest)) =
        crate::parser::stmt::simple::l10n_match_adverb("adverb-pc", r)
        && canonical == "exists"
    {
        (rest, negated)
    } else if r.starts_with(":!exists") && !is_ident_char(r.as_bytes().get(8).copied()) {
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
    Some((r, exists_node(target, negated, arg, adverb)))
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
