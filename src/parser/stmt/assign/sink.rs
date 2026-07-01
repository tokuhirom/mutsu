use super::*;

pub(crate) fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
    // Try to parse a chained assignment: $var op= ...
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input) {
        // After a chained assign, check for comma list at this level
        let (r, _) = ws(rest)?;
        // A chained *list* assignment to an `@`/`%` container absorbs the comma
        // list on its right: in `@a = %h = 1, 2` the inner `%h =` grabs the whole
        // `1, 2`, giving `@a = (%h = (1, 2))`. `try_parse_assign_expr` parses the
        // inner RHS comma-blind (so `%h` takes only `1` and leaves `, 2`); fold the
        // trailing comma back into the inner assignment's RHS here rather than
        // letting it be collected as a sibling of the outer list (which threw an
        // odd-elements hash-init error / shifted a following container by one).
        // This is confined to the statement-level RHS path; `paren_expr` calls
        // `try_parse_assign_expr` directly and keeps its own (self-reference-safe)
        // handling for `(%h = a, b)`.
        if r.starts_with(',')
            && !r.starts_with(",,")
            && let Expr::AssignExpr {
                name,
                expr,
                is_bind: false,
            } = &assign_expr
            && (name.starts_with('@') || name.starts_with('%'))
        {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            if !(r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')'))
            {
                let mut items = vec![(**expr).clone()];
                let (mut r_iter, second) = expression(r2)?;
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
                    let (r3, next) = expression(r3)?;
                    items.push(next);
                    r_iter = r3;
                }
                return Ok((
                    r_iter,
                    Expr::AssignExpr {
                        name: name.clone(),
                        expr: Box::new(Expr::ArrayLiteral(items)),
                        is_bind: false,
                    },
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
            let (mut r, second) = expression(r)?;
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
                let (r2, next) = expression(r2)?;
                items.push(next);
                r = r2;
            }
        }
        return Ok((rest, assign_expr));
    }
    parse_comma_or_expr(input)
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
                }))),
                right,
            }))
        }
        _ => None,
    }
}
