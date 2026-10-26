use super::*;

pub(crate) fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
    // Try to parse a chained assignment: $var op= ...
    //
    // `try_parse_assign_expr` short-circuits a leading parenthesized
    // assignment (`($b = 2)`), stopping right after the `)`. When something
    // other than a clean terminator follows -- e.g. `($b = 2) / 2`, where `/`
    // still needs to bind to the whole group -- fall through to
    // `parse_comma_or_expr` below instead, which reaches the same
    // recognition through `paren_expr` (wrapping it in `Expr::Grouped`) and
    // then correctly continues the infix/postfix precedence chain, exactly
    // like the plain `=` statement path already does. See #6953.
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input)
        && (!input.starts_with('(') || paren_assign_rhs_is_complete(rest))
    {
        return Ok((rest, assign_expr));
    }
    parse_comma_or_expr(input)
}

/// No-word-logical twin of [`parse_assign_expr_or_comma`]: the same
/// chained-assignment recognition, then the comma handling, with every element parsed with
/// [`expression_no_word_logical`] so a top-level (unparenthesized) word-logical
/// is left in the stream for the statement-level word-logical tail. Used by the
/// assignment / declaration RHS parsers so `@a = 1, 2 and 3` is
/// `(@a = 1, 2) and 3` while `$x ||= 42, 43` keeps its item-assignment
/// (comma-tight) `||=`.
pub(crate) fn parse_assign_expr_or_comma_no_word_logical(input: &str) -> PResult<'_, Expr> {
    // See the matching comment in `parse_assign_expr_or_comma` above (#6953):
    // only take the parenthesized-assignment shortcut when nothing but a
    // clean terminator follows the `)`.
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input)
        && (!input.starts_with('(') || paren_assign_rhs_is_complete(rest))
    {
        let (r, _) = ws(rest)?;
        // Chained *list* assignment to an `@`/`%` container absorbs the comma list
        // on its right (see [`parse_assign_expr_or_comma`]).
        if r.starts_with(',')
            && !r.starts_with(",,")
            && let Some(expr) = list_assign_source(&assign_expr)
        {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            if !(r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')'))
            {
                let mut items = vec![expr.clone()];
                let (mut r_iter, second) = expression_no_word_logical(r2)?;
                items.push(second);
                loop {
                    let (r3, _) = ws(r_iter)?;
                    if !r3.starts_with(',') || r3.starts_with(",,") {
                        r_iter = r3;
                        break;
                    }
                    let (r3, _) = parse_char(r3, ',')?;
                    let (r3, _) = ws(r3)?;
                    if r3.starts_with(';') || r3.is_empty() || r3.starts_with('}') {
                        r_iter = r3;
                        break;
                    }
                    let (r3, next) = expression_no_word_logical(r3)?;
                    items.push(next);
                    r_iter = r3;
                }
                return Ok((
                    r_iter,
                    with_list_source(assign_expr, Expr::ArrayLiteral(items)),
                ));
            }
        }
        if r.starts_with(',') && !r.starts_with(",,") {
            let (r, _) = parse_char(r, ',')?;
            let (r, _) = ws(r)?;
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
                return Ok((r, Expr::ArrayLiteral(vec![assign_expr])));
            }
            let mut items = vec![assign_expr];
            let (mut r, second) = expression_no_word_logical(r)?;
            items.push(second);
            loop {
                let (r2, _) = ws(r)?;
                if !r2.starts_with(',') {
                    return Ok((r2, Expr::ArrayLiteral(items)));
                }
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                    return Ok((r2, Expr::ArrayLiteral(items)));
                }
                let (r2, next) = expression_no_word_logical(r2)?;
                items.push(next);
                r = r2;
            }
        }
        return Ok((rest, assign_expr));
    }
    parse_comma_or_expr_no_word_logical(input)
}

pub(crate) fn rewrite_scalar_assignment_rhs_as_sink(name: String, rhs: Expr) -> Option<Expr> {
    match rhs {
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if name.starts_with('$') && matches!(meta.as_str(), "X" | "Z") => Some(Expr::MetaOp {
            meta,
            op,
            left: Box::new(Expr::AssignExpr {
                name,
                expr: left,
                is_bind: false,
            }),
            right,
        }),
        _ => None,
    }
}

pub(crate) fn rewrite_scalar_assignment_stmt_as_sink(name: String, rhs: Expr) -> Option<Stmt> {
    match rhs {
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if name.starts_with('$') && matches!(meta.as_str(), "X" | "Z") => {
            Some(Stmt::Expr(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::DoStmt(Box::new(Stmt::Assign {
                    name,
                    expr: *left,
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
                }))),
                right,
            }))
        }
        _ => None,
    }
}

/// The source of a list assignment to an `@`/`%` variable, which absorbs the
/// comma list on its right (`@a = %h = 1, 2`).
// Cost: O(1).
fn list_assign_source(expr: &Expr) -> Option<&Expr> {
    match expr {
        Expr::AssignExpr {
            name,
            expr,
            is_bind: false,
        } if name.starts_with('@') || name.starts_with('%') => Some(expr),
        Expr::Ast(node) => match node.as_ref() {
            crate::ast::parsed::ParsedExpr::Assign(assign)
                if matches!(assign.target, Expr::ArrayVar(_) | Expr::HashVar(_)) =>
            {
                Some(&assign.source)
            }
            _ => None,
        },
        _ => None,
    }
}

/// The list assignment [`list_assign_source`] read, assigning `source`.
// Cost: O(1).
fn with_list_source(assign: Expr, source: Expr) -> Expr {
    match assign {
        Expr::AssignExpr { name, .. } => Expr::AssignExpr {
            name,
            expr: Box::new(source),
            is_bind: false,
        },
        Expr::Ast(node) => match *node {
            crate::ast::parsed::ParsedExpr::Assign(assign) => {
                super::written_assignment(assign.target, source)
            }
            other => Expr::Ast(Box::new(other)),
        },
        other => other,
    }
}
