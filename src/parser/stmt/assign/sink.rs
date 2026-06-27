use super::*;

pub(crate) fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
    // Try to parse a chained assignment: $var op= ...
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input) {
        // After a chained assign, check for comma list at this level
        let (r, _) = ws(rest)?;
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
