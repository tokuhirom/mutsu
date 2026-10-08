use super::*;

/// The set operators that have an `OP=` spelling, in the order they are tried.
const SET_COMPOUND_OPS: [(&str, TokenKind); 12] = [
    ("(|)", TokenKind::SetUnion),
    ("(&)", TokenKind::SetIntersect),
    ("(.)", TokenKind::SetMultiply),
    ("(-)", TokenKind::SetDiff),
    ("(^)", TokenKind::SetSymDiff),
    ("(+)", TokenKind::SetAddition),
    ("\u{228E}", TokenKind::SetAddition),
    ("\u{222A}", TokenKind::SetUnion),
    ("\u{2229}", TokenKind::SetIntersect),
    ("\u{228D}", TokenKind::SetMultiply),
    ("\u{2216}", TokenKind::SetDiff),
    ("\u{2296}", TokenKind::SetSymDiff),
];

/// The set operator written `spelling` (`(|)`, `\u{222A}`, ...), without its `=`.
// Cost: O(1).
pub(crate) fn set_op_from_spelling(spelling: &str) -> Option<TokenKind> {
    SET_COMPOUND_OPS
        .iter()
        .find(|(candidate, _)| *candidate == spelling)
        .map(|(_, tok)| tok.clone())
}

/// Parse set operator compound assignment: `(|)=`, `(&)=`, `(-)=`, `(^)=`, `(.)=`, `(+)=`
/// and their Unicode variants: `\u{222A}=`, `\u{2229}=`, etc.
/// Returns (rest_after_equals, TokenKind for the set operator, the operator as written).
pub(crate) fn parse_set_compound_assign_op(input: &str) -> Option<(&str, TokenKind, &str)> {
    let (spelling, tok) = SET_COMPOUND_OPS
        .iter()
        .find(|(spelling, _)| input.starts_with(spelling))?;
    let after_op = &input[spelling.len()..];
    if after_op.starts_with('=') && !after_op.starts_with("==") {
        Some((&after_op[1..], tok.clone(), &input[..spelling.len()]))
    } else {
        None
    }
}

/// `LVALUE OP= RHS` for a set operator, as the compiler runs it: a subscript
/// target evaluates its index once, a method-call target writes back through
/// the method, anything else is `LVALUE = LVALUE OP RHS`.
pub(crate) fn build_set_compound_assign_expr(expr: Expr, set_tok: TokenKind, rhs: Expr) -> Expr {
    if let Expr::Index {
        target,
        index,
        is_positional,
        spelling,
    } = &expr
    {
        let tmp_idx = format!(
            "__mutsu_idx_{}",
            TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
        );
        let tmp_idx_expr = Expr::Var(tmp_idx.clone());
        let lhs_expr = Expr::Index {
            target: target.clone(),
            index: Box::new(tmp_idx_expr.clone()),
            is_positional: *is_positional,
            spelling: Default::default(),
        };
        let assigned_value = Expr::Binary {
            left: Box::new(autoviv_set_compound_lhs(lhs_expr, &set_tok)),
            op: set_tok,
            right: Box::new(rhs),
            form: Default::default(),
        };
        return Expr::desugar_block(vec![
            Stmt::VarDecl {
                name: tmp_idx,
                expr: (*index.clone()),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::IndexAssign {
                target: target.clone(),
                index: Box::new(tmp_idx_expr),
                value: Box::new(assigned_value),
                is_positional: *is_positional,
                spelling: *spelling,
            }),
        ]);
    }
    if let Expr::MethodCall {
        ref target,
        ref name,
        ref args,
        ref modifier,
        ..
    } = expr
    {
        let assigned_value = Expr::Binary {
            left: Box::new(autoviv_set_compound_lhs(expr.clone(), &set_tok)),
            op: set_tok,
            right: Box::new(rhs),
            form: Default::default(),
        };
        let topic_name = if let Expr::Var(ref v) = **target {
            v.clone()
        } else {
            "_".to_string()
        };
        let method_name = if let Some(m @ ('!' | '^')) = *modifier {
            format!("{m}{}", name.resolve())
        } else {
            name.resolve()
        };
        return Expr::Call {
            name: Symbol::intern("__mutsu_assign_method_lvalue"),
            args: vec![
                (**target).clone(),
                Expr::Literal(crate::value::Value::str(method_name)),
                Expr::ArrayLiteral(args.clone()),
                assigned_value,
                Expr::Literal(crate::value::Value::str(topic_name)),
                Expr::Literal(crate::value::Value::truth(true)),
            ],
            listop: false,
        };
    }
    // A plain variable stores the combined value back.
    let variable = match &expr {
        Expr::Var(name) => Some(name.clone()),
        Expr::ArrayVar(name) => Some(format!("@{name}")),
        Expr::HashVar(name) => Some(format!("%{name}")),
        _ => None,
    };
    let combined = Expr::Binary {
        left: Box::new(autoviv_set_compound_lhs(expr, &set_tok)),
        op: set_tok,
        right: Box::new(rhs),
        form: Default::default(),
    };
    match variable {
        Some(name) => Expr::AssignExpr {
            name,
            expr: Box::new(combined),
            is_bind: false,
        },
        None => combined,
    }
}

/// [`build_set_compound_assign_expr`] under the marker that keeps the written
/// `LVALUE OP= RHS`; `spelling` is the operator as written, without its `=`.
pub(crate) fn preserve_set_compound_assign(
    lhs: Expr,
    spelling: &str,
    set_tok: TokenKind,
    rhs: Expr,
) -> Expr {
    let expanded = build_set_compound_assign_expr(lhs.clone(), set_tok, rhs.clone());
    Expr::CompoundAssign {
        target: Box::new(lhs),
        op: format!("{spelling}="),
        rhs: Box::new(rhs),
        expanded: Box::new(expanded),
    }
}
