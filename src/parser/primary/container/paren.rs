use crate::ast::Expr;
use crate::parser::expr::{expression, expression_no_sequence};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::token_kind::TokenKind;

use super::meta_ops::{
    finalize_paren_list, maybe_curry_xz_metaop, normalize_chained_zip_meta,
    normalize_sequence_waypoints, starts_with_sequence_op, try_inline_modifier,
    try_parse_sequence_in_paren,
};

/// True when `expr` is an already-built WhateverCode closure (e.g. from `*.so`
/// or `* + 1`), which is the value an extra paren layer should freeze.
fn is_whatevercode_closure(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Lambda {
            is_whatever_code: true,
            ..
        } | Expr::AnonSubParams {
            is_whatever_code: true,
            ..
        }
    )
}

/// True when `s` is exactly one balanced parenthesis group spanning the whole
/// (trimmed) string — i.e. the opening `(` matches the final `)`. Used to tell
/// `((*))` (content `(*)` is a whole group → freeze) from `((*) + 1)` (content
/// `(*) + 1` is not → stays a WhateverCode).
fn is_single_paren_group(s: &str) -> bool {
    let s = s.trim();
    if !s.starts_with('(') || !s.ends_with(')') {
        return false;
    }
    let mut depth = 0usize;
    let bytes = s.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        match b {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    return i == bytes.len() - 1;
                }
            }
            _ => {}
        }
    }
    false
}

/// Parse a parenthesized expression or list.
pub(crate) fn paren_expr(input: &str) -> PResult<'_, Expr> {
    // Try the comprehensive parenthesized assignment parser first.
    // This handles complex LHS forms like %hash{...}, @arr[...], method calls, etc.
    if let Ok((rest, assign_expr)) = crate::parser::stmt::assign::try_parse_assign_expr(input) {
        return Ok((rest, Expr::Grouped(Box::new(assign_expr))));
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
    // Chained colonpairs in parens: (:a(2) :b(3) :c(4)) → list of pairs.
    // In Raku, space-separated colonpairs inside parentheses form a list without commas.
    if is_colonpair_expr(&first) && looks_like_colonpair_start(input) {
        let mut items = vec![first];
        let mut rest = input;
        while looks_like_colonpair_start(rest) {
            let (r, pair) = crate::parser::primary::misc::colonpair_expr(rest)?;
            items.push(pair);
            let (r, _) = ws(r)?;
            rest = r;
        }
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, finalize_paren_list(items)));
    }
    let before_close = input;
    if let Ok((input, _)) = parse_char(input, ')') {
        // Parenthesized pair: (:a(3)) — mark as positional so it's not treated
        // as a named argument in function calls.
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
            curried @ (Expr::Lambda { .. } | Expr::AnonSubParams { .. }) => curried,
            other => normalize_chained_zip_meta(other),
        };
        // Wrap in Grouped so the compiler's chain-flattener can
        // distinguish `(1|2)|3` from `1|2|3` for junction operators.
        //
        // A lone parenthesized bareword (a listop term such as `(done)`) is also
        // wrapped: the parens close off the listop so it cannot gobble a trailing
        // operator, which lets `1 ?? (done) !! 2` parse (matching Rakudo). Without
        // the wrapper the ternary then-branch check would reject the naked
        // `BareWord`, conflating `(done)` with the gobbling `done`.
        let result = if matches!(
            &result,
            Expr::Binary {
                op: TokenKind::Pipe | TokenKind::Ampersand | TokenKind::Caret,
                ..
            }
        ) || matches!(&result, Expr::BareWord(_))
            // A parenthesized feed (`(@a ==> grep ...)`) must stay wrapped so an
            // enclosing `my @g = (...)` does NOT split it (the parens isolate the
            // feed: `my @g = (@a ==> grep)` assigns the feed result, whereas
            // `my @g = @a ==> grep` binds `=` tighter and feeds `(my @g = @a)`).
            || matches!(&result, Expr::Feed { .. })
        {
            Expr::Grouped(Box::new(result))
        } else {
            result
        };
        // Whatever-currying freeze: a `*` (or a `*`-curried WhateverCode) wrapped
        // in an *extra* layer of parens becomes a frozen value, not a curry point.
        // Raku: `(*).Capture` curries to a WhateverCode, but `((*)).Capture` calls
        // `.Capture` on the literal Whatever (and throws). One level of parens is
        // transparent to currying; a second freezes. We detect the second level by
        // checking that the parenthesized content is itself a single, whole paren
        // group (`(*)`, `(*.so)`, `((*))`) rather than a larger expression that
        // merely begins with a paren (`(*) + 1`, which stays a WhateverCode).
        let consumed = &content_start[..content_start.len() - before_close.len()];
        let result = if is_single_paren_group(consumed)
            && (crate::parser::expr::is_whatever(&result)
                || is_whatevercode_closure(&result)
                || matches!(&result, Expr::Grouped(_)))
        {
            Expr::Grouped(Box::new(result))
        } else {
            result
        };
        // `($a) = ...` — a parenthesized single scalar directly followed by an
        // item-assignment `=` is a LIST assignment: the lone target slurps the
        // whole RHS as an itemized list (`($a) = 1, 2, 3` → `$a` is `$(1, 2, 3)`,
        // `.elems` 3), unlike a bare `$a = 1, 2, 3` (item assignment, `$a` is 1).
        // Wrap in `Grouped` so the `=` handler (logic.rs) keeps the
        // comma-absorbing list-assignment RHS instead of the item-assignment one.
        let result = if matches!(&result, Expr::Var(_)) {
            let after = input.trim_start();
            if after.starts_with('=')
                && !after.starts_with("==")
                && !after.starts_with("=>")
            {
                Expr::Grouped(Box::new(result))
            } else {
                result
            }
        } else {
            result
        };
        return Ok((input, result));
    }
    // Comma-separated list with sequence operator detection
    // Use expression_no_sequence so that `...` is not consumed as part of an item
    let sep = if input.starts_with(',') {
        ','
    } else if input.starts_with(';') && !input.starts_with(";;") {
        ';'
    } else {
        return Err(PError::expected("',' or ';' in parenthesized list"));
    };
    let (input, _) = parse_char(input, sep)?;
    let (input, _) = ws(input)?;
    // A top-level `;` inside `(...)` separates the list into "sections", one per
    // semicolon-group: `(1,2;3,4)` is `((1,2),(3,4))`, not the flat `(1,2,3,4)`.
    // `items` accumulates the CURRENT section; completed sections move into
    // `sections`. With no semicolon present, the result is the usual flat list.
    let mut sections: Vec<Vec<Expr>> = Vec::new();
    let mut items = vec![first];
    let mut saw_semicolon = false;
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
    // Handle trailing comma/semicolon before close paren
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((
            input,
            finalize_paren_sections(sections, items, saw_semicolon),
        ));
    }
    let (mut input_rest, second) = expression_no_sequence(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
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
    // Must start with identifier char or `!` (negated colonpair)
    r.starts_with(|c: char| c.is_alphabetic() || c == '_' || c == '!')
}
