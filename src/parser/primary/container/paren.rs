use crate::ast::Expr;
use crate::parser::expr::{expression, expression_no_sequence};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;
use std::collections::HashMap;

/// `X::Syntax::Adverb`: a colonpair immediately follows a term that cannot
/// take one. `spelled` is the term's own source text and appears in both the
/// message and the `.what` attribute.
fn adverb_not_allowed_error(spelled: &str) -> PError {
    let message = format!("You can't adverb {spelled}");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("what".to_string(), Value::str(spelled.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Syntax::Adverb"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

use super::meta_ops::{
    finalize_paren_list, maybe_curry_xz_metaop, normalize_chained_zip_meta,
    normalize_sequence_waypoints, starts_with_sequence_op, try_inline_modifier,
    try_parse_sequence_in_paren,
};

/// Parse a parenthesized expression or list.
pub(crate) fn paren_expr(input: &str) -> PResult<'_, Expr> {
    // Parens open a fresh nesting context: nothing inside binds to a prefix
    // operator waiting outside the group.
    let (rest, expr) = crate::parser::expr::without_pending_prefix(|| paren_expr_inner(input))?;
    Ok((rest, mark_parenthesized(expr)))
}

/// Record that the source wrote parentheses around `expr`.
///
/// The parser used to add this marker only for a narrow allowlist of shapes,
/// each entry buying one specific downstream behaviour, which left the AST
/// unable to say what raku says: rakudo models any `(...)` as
/// `Circumfix::Parentheses(SemiList(...))`. The marker is now unconditional, so
/// a parenthesization is a property of the *source*, not of who happens to care
/// about it. `Grouped` stays transparent to the compiler; the consumers that
/// read it read the same thing they always did, just in more places.
///
/// Two shapes place the marker differently:
///
/// - `PositionalPair` keeps the marker on the *inside*. Every call-argument path
///   keys on `PositionalPair` being outermost to tell `f((a => 1))` (a
///   positional `Pair`) from `f(a => 1)` (a named argument).
/// - An already-`Grouped` expression is wrapped again, because a second pair of
///   parentheses is a second `Circumfix::Parentheses` in rakudo — and, for
///   Whatever-currying, the layer that freezes the curry. `(*)` is a curry point
///   and `((*))` is a literal `Whatever` value; `(*.flip)` composes into an
///   enclosing curry while `((*.flip))` is a finished `WhateverCode` you can
///   call `.assuming` on. `parser::expr::is_frozen_whatever` reads that layer
///   count back, which is why the freeze needs no source-text rescan.
fn mark_parenthesized(expr: Expr) -> Expr {
    match expr {
        Expr::PositionalPair(inner) => {
            Expr::PositionalPair(Box::new(Expr::Grouped(Box::new(*inner))))
        }
        other => Expr::Grouped(Box::new(other)),
    }
}

fn paren_expr_inner(input: &str) -> PResult<'_, Expr> {
    // Try the comprehensive parenthesized assignment parser first.
    // This handles complex LHS forms like %hash{...}, @arr[...], method calls, etc.
    if let Ok((rest, assign_expr)) = crate::parser::stmt::assign::try_parse_assign_expr(input) {
        // `mark_parenthesized` adds the `Grouped` marker on the way out.
        return Ok((rest, assign_expr));
    }
    let (input, _) = parse_char(input, '(')?;
    let (input, _) = ws(input)?;
    let content_start = input;
    if let Ok((input, _)) = parse_char(input, ')') {
        // Empty parens = empty list
        return Ok((input, Expr::ArrayLiteral(Vec::new())));
    }
    // Try class declaration in parens: (class A { })
    if (input.starts_with("class ") || input.starts_with("class\t") || input.starts_with("class\n"))
        && let Ok((r, class_stmt)) = crate::parser::stmt::class::class_decl(input)
    {
        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ')') {
            return Ok((r, Expr::DoStmt(Box::new(class_stmt))));
        }
    }
    // Try temp/let statement in parens: (temp @a), (temp $x = 42), (let $x = 42)
    // Guard: only try if input starts with the keyword followed by whitespace or sigil.
    if (input.starts_with("temp ") || input.starts_with("temp\t") || input.starts_with("temp\n"))
        && let Ok((r, stmt)) = crate::parser::stmt::temp_stmt_pub(input)
    {
        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ')') {
            return Ok((r, Expr::DoStmt(Box::new(stmt))));
        }
    }
    if (input.starts_with("let ") || input.starts_with("let\t") || input.starts_with("let\n"))
        && let Ok((r, stmt)) = crate::parser::stmt::let_stmt_pub(input)
    {
        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ')') {
            return Ok((r, Expr::DoStmt(Box::new(stmt))));
        }
    }
    // Try assignment expression: ($var = expr), (@arr = expr), (%hash = expr), or compound forms.
    let (input, first) = if let Ok((r, var_expr)) = expression_no_sequence(input) {
        let (r2, _) = ws(r)?;
        let assign_target = match &var_expr {
            Expr::Var(name) => Some((name.clone(), Expr::Var(name.clone()))),
            Expr::ArrayVar(name) => Some((format!("@{}", name), Expr::ArrayVar(name.clone()))),
            Expr::HashVar(name) => Some((format!("%{}", name), Expr::HashVar(name.clone()))),
            Expr::BareWord(name) => Some((name.clone(), Expr::BareWord(name.clone()))),
            _ => None,
        };
        if let Some((assign_name, lhs_expr)) = assign_target {
            if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
                // Simple assignment: ($var = expr)
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(rhs),
                        is_bind: false,
                    },
                )
            } else if let Some(stripped) = r2.strip_prefix("::=").or_else(|| r2.strip_prefix(":="))
            {
                // Binding expression in parens: ($var := expr)
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(rhs),
                        is_bind: false,
                    },
                )
            } else if let Some((stripped, op)) =
                crate::parser::stmt::assign::parse_compound_assign_op(r2)
            {
                // Compound assignment: ($var += expr)
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(Expr::Binary {
                            left: Box::new(lhs_expr),
                            op: op.token_kind(),
                            right: Box::new(rhs),
                        }),
                        is_bind: false,
                    },
                )
            } else {
                (r, var_expr)
            }
        } else {
            // Non-variable LHS compound assignment (e.g. `(* *= 2)` for WhateverCode)
            if let Some((stripped, op)) = crate::parser::stmt::assign::parse_compound_assign_op(r2)
            {
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::Binary {
                        left: Box::new(var_expr),
                        op: op.token_kind(),
                        right: Box::new(rhs),
                    },
                )
            } else {
                (r, var_expr)
            }
        }
    } else {
        expression_no_sequence(input)?
    };
    let (input, _) = ws(input)?;
    // A colonpair directly following a bare literal is illegal -- rakudo lets an
    // adverb attach to a call (`foo :bar`, absorbed as a named arg) or even a
    // binary-operator call (`1+2 :foo` -> `infix:<+>(1, 2, :foo)`), but not to a
    // plain literal term (`(3 :foo)`, `("str" :foo)`). Diagnosed with the term's
    // own source text, matching rakudo's exact wording (verified against
    // `raku -e '(3 :foo)'`: "You can't adverb 3").
    if matches!(&first, Expr::Literal(_) | Expr::LiteralSrc(_, _))
        && looks_like_colonpair_start(input)
    {
        let spelled = content_start[..content_start.len() - input.len()].trim_end();
        return Err(adverb_not_allowed_error(spelled));
    }
    // If sequence syntax appears, try full expression parsing first.
    // This avoids mis-parsing cases like ("a"...* ~~ / z /) where
    // sequence is followed by another infix operator.
    if starts_with_sequence_op(input)
        && let Ok((r_full, full_expr)) = expression(content_start)
    {
        let (r_full_ws, _) = ws(r_full)?;
        if let Ok((r_after, _)) = parse_char(r_full_ws, ')') {
            return Ok((
                r_after,
                normalize_chained_zip_meta(normalize_sequence_waypoints(full_expr)),
            ));
        }
        // When content starts with nested parens, the full parse can already
        // consume the closing ')' of this paren expression (e.g. `(() ... *)`).
        if content_start.starts_with('(') && r_full_ws.is_empty() {
            return Ok((
                r_full_ws,
                normalize_chained_zip_meta(normalize_sequence_waypoints(full_expr)),
            ));
        }
    }
    // Check for inline statement modifier: ($_ with data), (expr if cond), etc.
    if let Some(result) = try_inline_modifier(input, first.clone()) {
        let (rest, modified_expr) = result?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, modified_expr));
    }
    // Check for sequence operator after single item: (1 ... 5)
    if let Some(seq) = try_parse_sequence_in_paren(input, std::slice::from_ref(&first)) {
        return seq;
    }
    if let Ok((input, _)) = parse_char(input, ')') {
        // Parenthesized pair: (:a(3)) — mark as positional so it's not treated
        // as a named argument in function calls. The `Grouped` marker that
        // records the parentheses is added by `mark_parenthesized` on the way
        // out, *inside* this wrapper: the `PositionalPair` marker every
        // call-argument path keys on must stay outermost.
        let first = if matches!(
            &first,
            Expr::Binary {
                op: TokenKind::FatArrow,
                ..
            }
        ) {
            Expr::PositionalPair(Box::new(first))
        } else {
            first
        };
        // Curry BEFORE normalizing chained Z meta-ops: `1 Z+ * Z+ 3` must become
        // a WhateverCode wrapping the nested meta-op, not a (non-curryable) `zip`
        // call. A non-currying meta-op still normalizes as before.
        let result = match maybe_curry_xz_metaop(first) {
            // ADR-0033 Phase 1: a curried result is now a `WhateverCurry`
            // marker rather than a built `Lambda`/`AnonSubParams` closure;
            // keep it out of `normalize_chained_zip_meta` just the same.
            curried @ (Expr::WhateverCurry(_)
            | Expr::Lambda { .. }
            | Expr::AnonSubParams { .. }) => curried,
            other => normalize_chained_zip_meta(other),
        };
        // Every one of the behaviours that used to be bought here by a narrow
        // allowlist — the junction chain-flattener boundary, the listop-closing
        // `(done)`, the isolated feed, `($a) = 1,2,3` as a list assignment, the
        // tight parenthesized assignment, and the X/Z meta-op argument boundary
        // — is now bought by the *unconditional* marker `mark_parenthesized`
        // adds on the way out. The parentheses are recorded because the source
        // wrote them, not because one downstream consumer asked for them.
        //
        return Ok((input, result));
    }
    // Comma-separated list with sequence operator detection
    // Use expression_no_sequence so that `...` is not consumed as part of an item
    //
    // A top-level `;` inside `(...)` separates the list into "sections", one per
    // semicolon-group: `(1,2;3,4)` is `((1,2),(3,4))`, not the flat `(1,2,3,4)`.
    // `items` accumulates the CURRENT section; completed sections move into
    // `sections`. With no semicolon present, the result is the usual flat list.
    let mut sections: Vec<Vec<Expr>> = Vec::new();
    let mut items = vec![first];
    let mut saw_semicolon = false;
    let mut input_rest = input;
    loop {
        let (input, _) = ws(input_rest)?;
        // Space-separated colonpairs form a list without commas: (:$a :$b),
        // (:a(1) :b(2) :c(3)). A colonpair immediately followed by another
        // colonpair (no separating comma) continues the current list.
        if items.last().is_some_and(is_colonpair_expr) && looks_like_colonpair_start(input) {
            let (r, pair) = crate::parser::primary::misc::colonpair_expr(input)?;
            items.push(pair);
            input_rest = r;
            continue;
        }
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((
                input,
                finalize_paren_sections(sections, items, saw_semicolon),
            ));
        }
        // Check for sequence operator before comma (not inside a semicolon list)
        if !saw_semicolon && let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        // A statement modifier terminating the list: `(1, 2 if True, 3)`. The
        // content of `(...)` is a semilist of *statements*, so the whole
        // comma expression parsed so far is the modified statement and the
        // modifier's condition is itself a full comma expression — hence
        // `(1, 2 if True, 3)` is `(1, 2)` and `(1, 2 unless False, 3)` is
        // `Empty` (the condition `(False, 3)` is a truthy 2-element list).
        // The single-item spelling is handled before the loop; this is the
        // multi-item one, which used to die with "',' or ';' in parenthesized
        // list".
        if !saw_semicolon
            && let Some(result) = try_inline_modifier(input, finalize_paren_list(items.clone()))
        {
            let (rest, modified_expr) = result?;
            let (rest, _) = ws(rest)?;
            let (rest, _) = parse_char(rest, ')')?;
            return Ok((rest, modified_expr));
        }
        let sep = if input.starts_with(',') {
            ','
        } else if input.starts_with(';') && !input.starts_with(";;") {
            ';'
        } else {
            return Err(PError::expected("',' or ';' in parenthesized list"));
        };
        let (input, _) = parse_char(input, sep)?;
        let (input, _) = ws(input)?;
        if sep == ';' {
            saw_semicolon = true;
            sections.push(std::mem::take(&mut items));
        }
        if !saw_semicolon
            && let Some(result) = try_inline_modifier(input, finalize_paren_list(items.clone()))
        {
            let (rest, modified_expr) = result?;
            let (rest, _) = ws(rest)?;
            let (rest, _) = parse_char(rest, ')')?;
            return Ok((rest, modified_expr));
        }
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((
                input,
                finalize_paren_sections(sections, items, saw_semicolon),
            ));
        }
        let (input, next) = expression_no_sequence(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Combine semicolon-separated sections of a parenthesized list. With no
/// semicolon seen the list is flat (`finalize_paren_list`). Otherwise each
/// non-empty section becomes one element: a multi-item section is a sub-list,
/// a single-item section is the bare item. A single overall section (e.g. a
/// trailing `;`: `(1,2,3;)`) is NOT wrapped, matching Raku.
fn finalize_paren_sections(
    mut sections: Vec<Vec<Expr>>,
    current: Vec<Expr>,
    saw_semicolon: bool,
) -> Expr {
    if !saw_semicolon {
        return finalize_paren_list(current);
    }
    if !current.is_empty() {
        sections.push(current);
    }
    match sections.len() {
        0 => Expr::ArrayLiteral(Vec::new()),
        1 => build_paren_section(sections.into_iter().next().unwrap()),
        _ => Expr::ArrayLiteral(sections.into_iter().map(build_paren_section).collect()),
    }
}

/// Render one semicolon-section: a single item stays bare; multiple items form
/// a sub-list (via `finalize_paren_list`, so meta-ops etc. still lift).
fn build_paren_section(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        finalize_paren_list(items)
    }
}

/// Check if an expression is a colonpair (represented as a FatArrow binary expression).
fn is_colonpair_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary {
            op: TokenKind::FatArrow,
            ..
        }
    )
}

/// Check if the input starts with a colonpair pattern (`:name`, `:!name`, `:name(...)`, etc.)
/// but not `::` (namespace separator) or `:=` (binding) or `:N<radix>` (radix literal).
fn looks_like_colonpair_start(input: &str) -> bool {
    let Some(r) = input.strip_prefix(':') else {
        return false;
    };
    if r.starts_with(':') || r.starts_with('=') {
        return false;
    }
    // :36<...> is a radix literal, not a colonpair
    let digit_end = r
        .char_indices()
        .take_while(|(_, c)| crate::builtins::unicode::unicode_decimal_digit_value(*c).is_some())
        .last()
        .map(|(idx, c)| idx + c.len_utf8())
        .unwrap_or(0);
    if digit_end > 0 && r[digit_end..].starts_with('<') {
        return false;
    }
    // Must start with an identifier char, `!` (negated colonpair), or a sigil
    // (`:$var`/`:@var`/`:%var`/`:&var` shorthand colonpair).
    r.starts_with(|c: char| {
        c.is_alphabetic() || c == '_' || c == '!' || c == '$' || c == '@' || c == '%' || c == '&'
    })
}
