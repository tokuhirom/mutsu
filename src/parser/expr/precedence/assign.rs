use super::*;
use crate::rakuast::assignment_ro_expr;

pub(crate) fn build_compound_assign_target_expr(target: Expr, op_name: &str, value: Expr) -> Expr {
    let target = unwrap_grouped_lvalue(target);
    if op_name == "=" {
        return assign_to_target_expr(target, value);
    }
    let Some(op) = compound_assign_op_from_name(op_name) else {
        return assignment_ro_expr(target, value);
    };
    // Bare `* op= value` curries to a WhateverCode that mutates its topic,
    // i.e. `{ $_ op= value }`. Used for e.g. `@a.map(* *= 2)`.
    if matches!(target, Expr::Whatever) {
        let body = build_compound_assign_target_expr(Expr::Var("_".to_string()), op_name, value);
        return Expr::Lambda {
            param: "_".to_string(),
            body: vec![crate::ast::Stmt::Expr(body)],
            is_whatever_code: true,
            param_sigilless: false,
        };
    }
    if let Expr::CompoundAssign { expanded, .. } = target {
        // The marker is transparent to the execution AST. A compound result
        // can still be the lvalue of a reverse meta compound assignment, so
        // continue through its established writeback expansion.
        return build_compound_assign_target_expr(*expanded, op_name, value);
    }
    match target {
        Expr::Var(name) => {
            short_circuit_compound_assign_expr(&name, Expr::Var(name.clone()), op, value.clone())
                .unwrap_or_else(|| Expr::AssignExpr {
                    name: name.clone(),
                    expr: Box::new(compound_assigned_value_expr(Expr::Var(name), op, value)),
                    is_bind: false,
                })
        }
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(compound_assigned_value_expr(
                Expr::ArrayVar(name),
                op,
                value,
            )),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(compound_assigned_value_expr(Expr::HashVar(name), op, value)),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => {
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: index.clone(),
                is_positional,
                spelling: Default::default(),
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
                is_positional: true,
                spelling,
            }
        }
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => {
            let lhs_expr = Expr::MultiDimIndex {
                target: target.clone(),
                dimensions: dimensions.clone(),
                is_positional,
            };
            Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
                is_positional,
            }
        }
        Expr::AssignExpr {
            name,
            expr,
            is_bind: _,
        } => {
            if op_name == "=" {
                return Expr::AssignExpr {
                    name,
                    expr: Box::new(value),
                    is_bind: false,
                };
            }
            let Some(op) = compound_assign_op_from_name(op_name) else {
                return assignment_ro_expr(
                    Expr::AssignExpr {
                        name,
                        expr,
                        is_bind: false,
                    },
                    value,
                );
            };
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(compound_assigned_value_expr(
                    Expr::AssignExpr {
                        name,
                        expr,
                        is_bind: false,
                    },
                    op,
                    value,
                )),
                is_bind: false,
            }
        }
        Expr::MethodCall {
            target, name, args, ..
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL));
            build_compound_assign_target_expr(
                Expr::Index {
                    target,
                    index: Box::new(index),
                    is_positional: true,
                    spelling: Default::default(),
                },
                op_name,
                value,
            )
        }
        // `(cond ?? A !! B) op= rhs`: apply the compound assignment through the
        // ternary lvalue to whichever branch is selected. See the matching arm in
        // `build_compound_assign_expr` (compound_expr.rs) for the rationale.
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Expr::Ternary {
            cond,
            then_expr: Box::new(build_compound_assign_target_expr(
                *then_expr,
                op_name,
                value.clone(),
            )),
            else_expr: Box::new(build_compound_assign_target_expr(
                *else_expr, op_name, value,
            )),
        },
        // A shape only the RakuAST expansion knows (`($s = $x if $c) ~= $y`, a
        // statement-modifier conditional with an lvalue body): that expansion
        // also answers `assignment_ro_expr` for what is not an lvalue.
        other => crate::rakuast::compound::expand_compound_assign_expr(
            other.clone(),
            op_name,
            value.clone(),
        )
        .unwrap_or_else(|_| assignment_ro_expr(other, value)),
    }
}

/// A `*`-slurpy parenthesized list-lvalue with exactly one real (non-`*`)
/// target extracts just that target's element from the RHS by position,
/// rather than assigning the RHS whole.
pub(crate) fn single_target_list_lvalue_expr(items: Vec<Expr>, rhs: Expr) -> Option<Expr> {
    let mut saw_whatever = false;
    let mut lvalues: Vec<(usize, Expr)> = Vec::new();
    for (i, item) in items.iter().enumerate() {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push((i, item.clone()));
    }
    if !saw_whatever || lvalues.len() != 1 {
        return None;
    }
    let (pos, target) = lvalues.into_iter().next()?;
    // Extract the element at position `pos` from the RHS list for scalar/hash
    // targets; an array target instead gets the (possibly skipped) tail —
    // see the matching comment in `single_target_list_lvalue_stmt`.
    let extracted_rhs = Expr::Index {
        target: Box::new(rhs.clone()),
        index: Box::new(Expr::Literal(Value::int(pos as i64))),
        is_positional: true,
        spelling: Default::default(),
    };
    Some(match target {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(extracted_rhs),
            is_bind: false,
        },
        Expr::ArrayVar(name) => {
            let array_rhs = if pos > 0 {
                Expr::MethodCall {
                    target: Box::new(rhs),
                    name: Symbol::intern("skip"),
                    args: vec![Expr::Literal(Value::int(pos as i64))],
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }
            } else {
                rhs
            };
            Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(Expr::Call {
                    name: Symbol::intern("__mutsu_star_lvalue_rhs"),
                    args: vec![Expr::Literal(Value::str(format!("@{}", name))), array_rhs],
                    listop: false,
                }),
                is_bind: false,
            }
        }
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(extracted_rhs),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(extracted_rhs),
            is_positional,
            spelling,
        },
        _ => return None,
    })
}

/// The right-hand side of a compound assignment `op=` in expression position.
///
/// Rakudo gives the assignment metaop item-assignment precedence -- tighter
/// than the comma -- whenever its base operator is tighter than the comma, so
/// `($x += 5, 9)` is `(($x += 5), 9)`, `f($x //= 1, 2)` passes two arguments,
/// and `@a += 5, 9` adds 5, whatever the lvalue's sigil. Only a base operator
/// as loose as the comma itself (`,=`) takes the comma list as its operand.
pub(crate) fn parse_compound_assign_rhs_mode(
    input: &str,
    comma_level: bool,
    mode: ExprMode,
) -> PResult<'_, Expr> {
    if comma_level {
        parse_assignment_rhs_mode(input, mode)
    } else {
        item_expr(input, mode)
    }
}

pub(crate) fn parse_assignment_rhs_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    // Parse each comma element at the *list-infix* level (`list_infix_top`):
    // everything tighter than the comma plus the list-infix operators (`Z`/`X`/
    // their meta-ops, the sequence `...`, `minmax`, feed), so `@a[^3] = 1, 2 ... 10`
    // still folds the whole comma list into one sequence. Crucially this stops
    // BEFORE the loose word-logicals (`and`/`or`/`xor`/`andthen`/`orelse`/
    // `notandthen`) — the loosest operators in Raku — so an assignment RHS never
    // absorbs them: `@a[0] = 8 andthen 0` is `(@a[0] = 8) andthen 0`, not
    // `@a[0] = (8 andthen 0)`. The trailing word-logical is left in the stream for
    // the enclosing `and`/`or` chain, which re-attaches it with the whole
    // assignment as its left operand. (`ternary_mode` used to be called here, but
    // it wrongly folded the word-logical into the RHS, storing the tail value;
    // see logic.rs `assign_not_expr_mode`.)
    let (rest, first) = list_infix_top(input, mode)?;
    let (r, _) = ws(rest)?;
    if !r.starts_with(',') || r.starts_with(",,") {
        return Ok((rest, first));
    }

    let mut items = vec![first];
    let mut cursor = r;
    loop {
        let (r2, _) = parse_char(cursor, ',')?;
        let (r2, _) = ws(r2)?;
        if r2.is_empty() || r2.starts_with(';') || r2.starts_with('}') || r2.starts_with(')') {
            return Ok((r2, finalize_assignment_rhs_list(items)));
        }
        let (r3, next) = list_infix_top(r2, mode)?;
        items.push(next);
        let (r3, _) = ws(r3)?;
        if !r3.starts_with(',') || r3.starts_with(",,") {
            return Ok((r3, finalize_assignment_rhs_list(items)));
        }
        cursor = r3;
    }
}

/// Build the RHS expression from a comma-separated assignment list.
///
/// The sequence operators (`...`/`…`/`...^`/`…^`) and list-infix meta-ops are
/// LOOSER than comma, so `@a[^5] = 1.5, 2.5 ... 5.5` collapses the whole comma
/// list into ONE sequence (seed `1.5, 2.5`), exactly like the declaration form
/// `my @a = 1.5, 2.5 ... 5.5` (which routes through `parse_comma_or_expr`) and
/// the argument-list case (#4755). `normalize_comma_list_items` folds the
/// preceding seeds into the trailing sequence/meta-op; when it collapses to a
/// single such expression we return it directly, otherwise keep the list.
fn finalize_assignment_rhs_list(items: Vec<Expr>) -> Expr {
    let original_len = items.len();
    let normalized = crate::parser::stmt::assign::normalize_comma_list_items(items);
    if normalized.len() == 1 && original_len > 1 {
        return normalized.into_iter().next().unwrap();
    }
    Expr::ArrayLiteral(normalized)
}
