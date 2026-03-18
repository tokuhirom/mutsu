use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_char, parse_tag};
use super::super::stmt::assign::{
    compound_assign_op_from_name, compound_assigned_value_expr, parse_assign_expr_or_comma,
    parse_meta_compound_assign_op,
};

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::operators::*;
use super::precedence_meta_ops::{
    BracketInfix, additive_expr, concat_expr, multiplicative_expr, op_str_to_token_kind,
    parse_bracket_infix_op, parse_infix_func_op, parse_meta_op, power_expr, strip_sequence_op,
    structural_expr,
};
use super::{contains_whatever, wrap_whatevercode};

static CHAIN_CMP_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Ternary: expr ?? expr !! expr
pub(super) fn ternary(input: &str) -> PResult<'_, Expr> {
    ternary_mode(input, ExprMode::Full)
}

pub(super) fn ternary_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, cond) = or_expr_no_assign_mode(input, mode)?;
    let (rest_ws, _) = ws(rest)?;
    if let Ok((input, _)) = parse_tag(rest_ws, "??") {
        let (input, _) = ws(input)?;
        // Parse then-expr at ternary precedence (stops before `!!`)
        let (input, then_expr) = if mode == ExprMode::Full {
            ternary_mode(input, mode).map_err(|err| {
                enrich_expected_error(err, "expected then-expression after '??'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        if mode == ExprMode::Full
            && let Expr::BareWord(name) = &then_expr
            && !name.contains("::")
        {
            return Err(PError::expected("expected '!!' in ternary expression"));
        }
        let (input, _) = ws(input)?;
        let (input, _) = if mode == ExprMode::Full {
            parse_tag(input, "!!").map_err(|err| {
                enrich_expected_error(err, "expected '!!' in ternary expression", input.len())
            })?
        } else {
            parse_tag(input, "!!")?
        };
        let (input, _) = ws(input)?;
        // Parse else-expr at ternary precedence (allows nested ternary)
        let (input, else_expr) = if mode == ExprMode::Full {
            ternary_mode(input, mode).map_err(|err| {
                enrich_expected_error(err, "expected else-expression after '!!'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        if let Expr::Binary { left, op, right } = cond {
            if matches!(
                op,
                TokenKind::AndAnd
                    | TokenKind::AndThen
                    | TokenKind::NotAndThen
                    | TokenKind::OrWord
                    | TokenKind::OrElse
            ) {
                return Ok((
                    input,
                    Expr::Binary {
                        left,
                        op,
                        right: Box::new(Expr::Ternary {
                            cond: right,
                            then_expr: Box::new(then_expr),
                            else_expr: Box::new(else_expr),
                        }),
                    },
                ));
            }
            return Ok((
                input,
                Expr::Ternary {
                    cond: Box::new(Expr::Binary { left, op, right }),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                },
            ));
        }
        return Ok((
            input,
            Expr::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ));
    }
    // No ternary operator: fall back to regular OR-level parsing, which includes
    // assignment expressions.
    or_expr_mode(input, mode)
}

/// Parse an operand at additive level or tighter (for loose prefix operators).
pub(super) fn loose_prefix_operand(input: &str, level: i32) -> PResult<'_, Expr> {
    use super::super::stmt::simple::*;
    if level <= PREC_CONCAT {
        concat_expr(input)
    } else {
        additive_expr(input)
    }
}

/// Parse an operand at multiplicative level.
pub(super) fn multiplicative_operand(input: &str) -> PResult<'_, Expr> {
    multiplicative_expr(input)
}

/// Parse an operand at power level.
pub(super) fn power_operand(input: &str) -> PResult<'_, Expr> {
    power_expr(input)
}

/// Low-precedence: or / xor / orelse
pub(super) fn or_expr(input: &str) -> PResult<'_, Expr> {
    or_expr_mode(input, ExprMode::Full)
}

fn or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_or_and_expr(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                assign_or_and_expr(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after 'or'/'xor'/'orelse'",
                        r.len(),
                    )
                })?
            } else {
                assign_or_and_expr(r, mode)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Low-precedence OR/XOR chain for ternary condition parsing.
/// This intentionally excludes assignment expressions so:
/// `a = b ?? c !! d` parses as `a = (b ?? c !! d)`.
fn or_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_expr_no_assign_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = and_expr_no_assign_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Assignment expressions at the or/and level: $var = expr
/// This sits between or/and and not in precedence.
fn assign_or_and_expr(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    and_expr_mode(input, mode)
}

/// Low-precedence: and / andthen / notandthen
fn and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                assign_not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after 'and'/'andthen'/'notandthen'",
                        r.len(),
                    )
                })?
            } else {
                assign_not_expr_mode(r, mode)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn and_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = not_expr_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn assign_not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, expr) = not_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    if !(r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>")) {
        return Ok((rest, expr));
    }

    let r = &r[1..];
    let (r, _) = ws(r)?;
    let (r, rhs) = parse_assignment_rhs_mode(r, mode)?;

    match expr {
        Expr::Var(name) => Ok((
            r,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
            },
        )),
        Expr::ArrayVar(name) => Ok((
            r,
            Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(rhs),
            },
        )),
        Expr::HashVar(name) => Ok((
            r,
            Expr::AssignExpr {
                name: format!("%{}", name),
                expr: Box::new(rhs),
            },
        )),
        Expr::Index { target, index } => {
            if let Expr::Call { name, args } = target.as_ref()
                && name == "__mutsu_subscript_adverb"
                && args.len() >= 3
                && matches!(index.as_ref(), Expr::Literal(Value::Int(1)))
                && matches!(&args[2], Expr::Literal(Value::Str(mode)) if mode.as_str() == "kv" || mode.as_str() == "not-kv")
            {
                return Ok((
                    r,
                    Expr::IndexAssign {
                        target: Box::new(args[0].clone()),
                        index: Box::new(args[1].clone()),
                        value: Box::new(rhs),
                    },
                ));
            }
            Ok((
                r,
                Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(rhs),
                },
            ))
        }
        Expr::Call { name, args } => Ok((
            r,
            Expr::Call {
                name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
                args: vec![
                    Expr::Literal(Value::str(name.resolve())),
                    Expr::ArrayLiteral(args),
                    rhs,
                ],
            },
        )),
        Expr::MultiDimIndex { target, dimensions } => Ok((
            r,
            Expr::IndexAssign {
                target,
                index: Box::new(Expr::ArrayLiteral(dimensions)),
                value: Box::new(rhs),
            },
        )),
        Expr::CallOn { target, args } => Ok((
            r,
            if args.is_empty() {
                if let Expr::ArrayLiteral(items) = *target.clone() {
                    if let Some(expr) = list_lvalue_assign_expr(items, rhs.clone()) {
                        expr
                    } else {
                        Expr::Call {
                            name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                            args: vec![*target, Expr::ArrayLiteral(args), rhs],
                        }
                    }
                } else {
                    Expr::Call {
                        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                        args: vec![*target, Expr::ArrayLiteral(args), rhs],
                    }
                }
            } else {
                Expr::Call {
                    name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                    args: vec![*target, Expr::ArrayLiteral(args), rhs],
                }
            },
        )),
        _ => Ok((rest, expr)),
    }
}

fn assignment_ro_expr(lhs: Expr, rhs: Expr) -> Expr {
    Expr::DoBlock {
        body: vec![
            Stmt::Expr(lhs),
            Stmt::Expr(rhs),
            Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_assignment_ro"),
                args: Vec::new(),
            }),
        ],
        label: None,
    }
}

fn assign_to_target_expr(target: Expr, value: Expr) -> Expr {
    match target {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(value),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(value),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(value),
        },
        Expr::Index { target, index } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(value),
        },
        Expr::MultiDimIndex { target, dimensions } => Expr::IndexAssign {
            target,
            index: Box::new(Expr::ArrayLiteral(dimensions)),
            value: Box::new(value),
        },
        Expr::Call { name, args } => Expr::Call {
            name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(name.resolve())),
                Expr::ArrayLiteral(args),
                value,
            ],
        },
        Expr::CallOn { target, args } => Expr::Call {
            name: Symbol::intern("__mutsu_assign_callable_lvalue"),
            args: vec![*target, Expr::ArrayLiteral(args), value],
        },
        Expr::DoStmt(stmt) => {
            if let Stmt::VarDecl {
                name,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
                ..
            } = *stmt
            {
                Expr::DoStmt(Box::new(Stmt::VarDecl {
                    name,
                    expr: value,
                    type_constraint,
                    is_state,
                    is_our,
                    is_dynamic,
                    is_export,
                    export_tags,
                    custom_traits,
                    where_constraint,
                }))
            } else {
                assignment_ro_expr(Expr::DoStmt(stmt), value)
            }
        }
        other => assignment_ro_expr(other, value),
    }
}

fn build_compound_assign_target_expr(target: Expr, op_name: &str, value: Expr) -> Expr {
    if op_name == "=" {
        return assign_to_target_expr(target, value);
    }
    let Some(op) = compound_assign_op_from_name(op_name) else {
        return assignment_ro_expr(target, value);
    };
    match target {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(compound_assigned_value_expr(Expr::Var(name), op, value)),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(compound_assigned_value_expr(
                Expr::ArrayVar(name),
                op,
                value,
            )),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(compound_assigned_value_expr(Expr::HashVar(name), op, value)),
        },
        Expr::Index { target, index } => {
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: index.clone(),
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
            }
        }
        Expr::MultiDimIndex { target, dimensions } => {
            let lhs_expr = Expr::MultiDimIndex {
                target: target.clone(),
                dimensions: dimensions.clone(),
            };
            Expr::IndexAssign {
                target,
                index: Box::new(Expr::ArrayLiteral(dimensions)),
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
            }
        }
        Expr::AssignExpr { name, expr } => {
            if op_name == "=" {
                return Expr::AssignExpr {
                    name,
                    expr: Box::new(value),
                };
            }
            let Some(op) = compound_assign_op_from_name(op_name) else {
                return assignment_ro_expr(Expr::AssignExpr { name, expr }, value);
            };
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(compound_assigned_value_expr(
                    Expr::AssignExpr { name, expr },
                    op,
                    value,
                )),
            }
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::Nil));
            build_compound_assign_target_expr(
                Expr::Index {
                    target,
                    index: Box::new(index),
                },
                op_name,
                value,
            )
        }
        other => assignment_ro_expr(other, value),
    }
}

fn list_lvalue_assign_expr(items: Vec<Expr>, rhs: Expr) -> Option<Expr> {
    let mut saw_whatever = false;
    let mut lvalues: Vec<Expr> = Vec::new();
    for item in items {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push(item);
    }
    if !saw_whatever || lvalues.len() != 1 {
        return None;
    }
    match lvalues.into_iter().next()? {
        Expr::Var(name) => Some(Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
        }),
        Expr::ArrayVar(name) => Some(Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
        }),
        Expr::HashVar(name) => Some(Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
        }),
        Expr::Index { target, index } => Some(Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
        }),
        _ => None,
    }
}

fn parse_assignment_rhs_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, first) = ternary_mode(input, mode)?;
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
            return Ok((r2, Expr::ArrayLiteral(items)));
        }
        let (r3, next) = ternary_mode(r2, mode)?;
        items.push(next);
        let (r3, _) = ws(r3)?;
        if !r3.starts_with(',') || r3.starts_with(",,") {
            return Ok((r3, Expr::ArrayLiteral(items)));
        }
        cursor = r3;
    }
}

fn not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    if input.starts_with("not")
        && !is_ident_char(input.as_bytes().get(3).copied())
        && !input[3..].starts_with('(')
        && !{
            let trimmed = input[3..].trim_start();
            trimmed.starts_with("=>") && !trimmed.starts_with("==>")
        }
    {
        let r = &input[3..];
        let (r, _) = ws(r)?;
        let (r, expr) = not_expr_mode(r, mode)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    if input.starts_with("so")
        && !is_ident_char(input.as_bytes().get(2).copied())
        && !input[2..].starts_with('(')
    {
        let r = &input[2..];
        let (r, _) = ws(r)?;
        let (r, expr) = not_expr_mode(r, mode)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    or_or_expr_mode(input, mode)
}

/// || , ^^ , and //
fn or_or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr_mode(input, mode)?;
    let mut last_list_assoc_op: Option<LogicalOp> = None;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated logical ops first: !|| , !^^
        let (negated, op_result) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::OrOr | LogicalOp::XorXor) {
                (true, Some((op, len)))
            } else {
                (false, None)
            }
        } else {
            (false, None)
        };
        let (op, len) = if let Some(pair) = op_result {
            pair
        } else if let Some(pair) = parse_or_or_op(r) {
            pair
        } else {
            break;
        };
        if matches!(op, LogicalOp::Min | LogicalOp::Max) {
            if let Some(prev) = last_list_assoc_op
                && prev != op
            {
                return Err(PError::fatal(format!(
                    "Only identical operators may be list associative; since '{}' and '{}' differ, they are non-associative and you need to clarify with parentheses",
                    match prev {
                        LogicalOp::Min => "min",
                        LogicalOp::Max => "max",
                        _ => unreachable!(),
                    },
                    match op {
                        LogicalOp::Min => "min",
                        LogicalOp::Max => "max",
                        _ => unreachable!(),
                    }
                )));
            }
            last_list_assoc_op = Some(op);
        }
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            if is_loose_not_or_so_prefix(r) {
                not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            } else {
                and_and_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            }
        } else if is_loose_not_or_so_prefix(r) {
            not_expr_mode(r, mode)?
        } else {
            and_and_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

fn is_loose_not_or_so_prefix(input: &str) -> bool {
    (input.starts_with("not")
        && !is_ident_char(input.as_bytes().get(3).copied())
        && !input[3..].starts_with('('))
        || (input.starts_with("so")
            && !is_ident_char(input.as_bytes().get(2).copied())
            && !input[2..].starts_with('('))
}

/// &&
fn and_and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated !&&
        let (negated, op, len) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::AndAnd) {
                (true, op, len)
            } else if let Some((op, len)) = parse_and_and_op(r) {
                (false, op, len)
            } else {
                break;
            }
        } else if let Some((op, len)) = parse_and_and_op(r) {
            (false, op, len)
        } else {
            break;
        };
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            comparison_expr_mode(r, mode).map_err(|err| {
                enrich_expected_error(err, "expected expression after '&&'", r.len())
            })?
        } else {
            comparison_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

/// Boolean bitwise / junction: ?| ?& ?^ | & ^
fn junctive_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let next_fn = if mode == ExprMode::Full {
        sequence_expr
    } else {
        list_infix_expr
    };
    let (mut rest, mut left) = next_fn(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_junctive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junctive operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Junction infix operators: |, &, ^
        if let Some((op, len)) = parse_junction_infix_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junction operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// List-infix operators only: Z, X, meta-ops, infix funcs.
/// Used in NoSequence mode (e.g. inside parenthesized expressions).
fn list_infix_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;
    rest = parse_list_infix_loop(rest, &mut left)?;
    Ok((rest, left))
}

/// Sequence (..., ...^) and list-infix (Z, X, meta-ops, infix funcs).
/// All have Raku precedence level f= (list associative).
fn sequence_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;

    // Helper: wrap LHS WhateverCode before building the sequence node.
    fn maybe_wrap_lhs(left: &mut Expr) {
        if contains_whatever(left) && !matches!(left, Expr::Whatever) {
            *left = wrap_whatevercode(left);
        }
    }

    loop {
        let (r, _) = ws(rest)?;

        // ...^ / …^ (exclusive-end sequence), ... / … (sequence)
        if let Some((r2, op, op_str)) = strip_sequence_op(r) {
            let (r2, _) = ws(r2)?;
            let (r2, mut right) =
                comparison_expr_mode(r2, ExprMode::NoSequence).map_err(|err| {
                    enrich_expected_error(
                        err,
                        format!("expected expression after '{op_str}'").as_str(),
                        r2.len(),
                    )
                })?;
            if contains_whatever(&right) && !matches!(right, Expr::Whatever) {
                right = wrap_whatevercode(&right);
            }
            maybe_wrap_lhs(&mut left);
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            rest = r2;
            continue;
        }
        // Try Z/X/meta/infix-func operators
        let new_rest = parse_list_infix_loop(rest, &mut left)?;
        if new_rest.len() < rest.len() {
            rest = new_rest;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Shared loop for Z/X meta operators and infix function calls.
/// Modifies `left` in place and returns the remaining input.
fn parse_list_infix_loop<'a>(input: &'a str, left: &mut Expr) -> Result<&'a str, PError> {
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with("X.") && !r.starts_with("X..") {
            let after = &r[2..];
            let after_trimmed = after.trim_start();
            if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
                return Err(PError::expected_at(
                    "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                    r,
                ));
            }
            return Err(PError::expected_at(
                "X::Syntax::CannotMeta: Cannot do . because it is too fiddly",
                r,
            ));
        }
        // Infixed function call: [&func], R[&func], X[&func], Z[&func]
        if let Some((modifier, name, len)) = parse_infix_func_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right_exprs) = if modifier.as_deref() == Some("X") {
                parse_comma_list_of_range(r)?
            } else {
                let (r, expr) = range_expr(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after infixed function",
                        r.len(),
                    )
                })?;
                (r, vec![expr])
            };
            *left = Expr::InfixFunc {
                name,
                left: Box::new(left.clone()),
                right: right_exprs,
                modifier,
            };
            rest = r;
            continue;
        }
        // Negated bracket infix operators: ![op]
        if let Some(r_after_bang) = r.strip_prefix('!')
            && let Some(bracket_infix) = parse_bracket_infix_op(r_after_bang)
        {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket infix op",
                            r.len(),
                        )
                    })?;
                    let binary = if let Some(tk) = op_str_to_token_kind(&op) {
                        Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        }
                    } else {
                        Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }
                    };
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(binary),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket meta infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::MetaOp {
                            meta,
                            op,
                            left: Box::new(left.clone()),
                            right: Box::new(right),
                        }),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::InfixFunc {
                            name,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }),
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Bracket infix operators: [+], [R-], [Z*], [Z[cmp]], [blue], etc.
        if let Some(bracket_infix) = parse_bracket_infix_op(r) {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket infix op",
                            r.len(),
                        )
                    })?;
                    // Resolve plain op to a Binary expr
                    if let Some(tk) = op_str_to_token_kind(&op) {
                        *left = Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        };
                    } else {
                        // Unknown plain op: treat as user-defined infix
                        *left = Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        };
                    }
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let needs_comma_list = meta == "Z"
                        || meta == "X"
                        || op == "..."
                        || op == "...^"
                        || op == "…"
                        || op == "…^";
                    let (r, right) = if needs_comma_list {
                        let (r, items) = parse_comma_list_of_range_raw(r)?;
                        if items.len() == 1 {
                            (r, items.into_iter().next().unwrap())
                        } else {
                            (r, Expr::ArrayLiteral(items))
                        }
                    } else {
                        range_expr(r).map_err(|err| {
                            enrich_expected_error(
                                err,
                                "expected expression after bracket meta infix op",
                                r.len(),
                            )
                        })?
                    };
                    let (r, right) = if meta == "X" {
                        let (r_ws, _) = ws(r)?;
                        if let Some(after_op) = r_ws.strip_prefix("~~") {
                            let (after_op, _) = ws(after_op)?;
                            let (r_after_rhs, rhs_expr) =
                                junctive_expr_mode(after_op, ExprMode::Full).map_err(|err| {
                                    enrich_expected_error(
                                        err,
                                        "expected expression after smartmatch",
                                        after_op.len(),
                                    )
                                })?;
                            (
                                r_after_rhs,
                                Expr::Binary {
                                    left: Box::new(right),
                                    op: TokenKind::SmartMatch,
                                    right: Box::new(rhs_expr),
                                },
                            )
                        } else {
                            (r_ws, right)
                        }
                    } else {
                        (r, right)
                    };
                    *left = Expr::MetaOp {
                        meta,
                        op,
                        left: Box::new(left.clone()),
                        right: Box::new(right),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::InfixFunc {
                        name,
                        left: Box::new(left.clone()),
                        right: vec![right],
                        modifier: None,
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Meta operators: R-, X+, Z~, bare Z, bare X, R[...], etc.
        if let Some((stripped, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
        {
            let (r, _) = ws(stripped)?;
            let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after reverse meta compound assignment",
                    r.len(),
                )
            })?;
            *left = build_compound_assign_target_expr(rhs, &op_name, left.clone());
            rest = r;
            continue;
        }
        if let Some((meta, op, len)) = parse_meta_op(r) {
            if meta == "X"
                && op == "cmp"
                && matches!(
                    &left,
                    Expr::MetaOp {
                        meta: prev_meta,
                        op: prev_op,
                        ..
                    } if prev_meta == "X" && prev_op == "cmp"
                )
            {
                return Err(PError::expected_at(
                    "Non-associative operator 'Xcmp' cannot be chained",
                    r,
                ));
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let needs_comma_list = (meta == "Z" || meta == "X")
                || op == "..."
                || op == "...^"
                || op == "…"
                || op == "…^";
            let (r, right) = if needs_comma_list {
                let (r, items) = parse_comma_list_of_range_raw(r)?;
                if items.len() == 1 {
                    (r, items.into_iter().next().unwrap())
                } else {
                    (r, Expr::ArrayLiteral(items))
                }
            } else {
                range_expr(r).map_err(|err| {
                    enrich_expected_error(err, "expected expression after meta operator", r.len())
                })?
            };
            let (r, right) = if meta == "X" {
                let (r_ws, _) = ws(r)?;
                if let Some(after_op) = r_ws.strip_prefix("~~") {
                    let (after_op, _) = ws(after_op)?;
                    let (r_after_rhs, rhs_expr) = junctive_expr_mode(after_op, ExprMode::Full)
                        .map_err(|err| {
                            enrich_expected_error(
                                err,
                                "expected expression after smartmatch",
                                after_op.len(),
                            )
                        })?;
                    (
                        r_after_rhs,
                        Expr::Binary {
                            left: Box::new(right),
                            op: TokenKind::SmartMatch,
                            right: Box::new(rhs_expr),
                        },
                    )
                } else {
                    (r_ws, right)
                }
            } else {
                (r, right)
            };
            *left = Expr::MetaOp {
                meta,
                op,
                left: Box::new(left.clone()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Flip-flop operators: ff/fff and endpoint-excluding forms.
        if let Some((name, len)) = parse_flipflop_infix(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = range_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after flip-flop operator", r.len())
            })?;
            *left = Expr::InfixFunc {
                name,
                left: Box::new(left.clone()),
                right: vec![right],
                modifier: None,
            };
            rest = r;
            continue;
        }
        // User-defined infix words (typically via my &infix:<...> = ...),
        // e.g. `42 same-in-Int "42"`.
        // Do not span statement boundaries across newlines.
        let ws_before = &rest[..rest.len() - r.len()];
        if !ws_before.contains('\n')
            && let Some((feed_op, len)) = parse_feed_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = (if matches!(feed_op, FeedOp::ToLeft | FeedOp::AppendLeft) {
                sequence_expr(r)
            } else {
                range_expr(r)
            })
            .map_err(|err| {
                enrich_expected_error(err, "expected expression after feed operator", r.len())
            })?;
            *left = build_feed_expr(feed_op, left.clone(), right);
            rest = r;
            continue;
        }
        if !ws_before.contains('\n')
            && let Some((name, len)) = parse_custom_infix_word(r)
            && super::super::stmt::simple::lookup_custom_infix_precedence(&name)
                .is_none_or(|level| level <= super::super::stmt::simple::PREC_SEQUENCE)
        {
            let mut r = &r[len..];
            let (r2, _) = ws(r)?;
            let (r2, right) = range_expr(r2).map_err(|err| {
                enrich_expected_error(err, "expected expression after infix operator", r.len())
            })?;
            let assoc = super::super::stmt::simple::lookup_user_infix_assoc(&name)
                .unwrap_or_else(|| "left".to_string());
            let mut args = vec![left.clone(), right];
            r = r2;
            if assoc != "left" {
                loop {
                    let (r_ws, _) = ws(r)?;
                    let ws_between = &r[..r.len() - r_ws.len()];
                    if ws_between.contains('\n') {
                        break;
                    }
                    let Some((next_name, next_len)) = parse_custom_infix_word(r_ws) else {
                        break;
                    };
                    if next_name != name {
                        break;
                    }
                    let r_after_op = &r_ws[next_len..];
                    let (r_after_op, _) = ws(r_after_op)?;
                    let (r_after_arg, arg) = range_expr(r_after_op).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after infix operator",
                            r_after_op.len(),
                        )
                    })?;
                    args.push(arg);
                    r = r_after_arg;
                }
            }
            *left = match assoc.as_str() {
                "right" => {
                    let mut iter = args.into_iter().rev();
                    let mut acc = iter
                        .next()
                        .unwrap_or(Expr::Literal(crate::value::Value::Nil));
                    for lhs in iter {
                        acc = Expr::InfixFunc {
                            name: name.clone(),
                            left: Box::new(lhs),
                            right: vec![acc],
                            modifier: None,
                        };
                    }
                    acc
                }
                "list" | "chain" => Expr::InfixFunc {
                    name: name.clone(),
                    left: Box::new(args[0].clone()),
                    right: args[1..].to_vec(),
                    modifier: None,
                },
                "non" => {
                    if args.len() > 2 {
                        return Err(PError::fatal(format!(
                            "Non-associative operator '{}' cannot be chained",
                            name
                        )));
                    }
                    Expr::InfixFunc {
                        name: name.clone(),
                        left: Box::new(args[0].clone()),
                        right: args[1..].to_vec(),
                        modifier: None,
                    }
                }
                _ => Expr::InfixFunc {
                    name: name.clone(),
                    left: Box::new(args[0].clone()),
                    right: args[1..].to_vec(),
                    modifier: None,
                },
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok(rest)
}

fn build_feed_expr(op: FeedOp, left: Expr, right: Expr) -> Expr {
    match op {
        FeedOp::ToRight => build_pipe_feed_expr(left, right),
        FeedOp::ToLeft => build_pipe_feed_expr(right, left),
        FeedOp::AppendRight => build_append_feed_expr(left, right),
        FeedOp::AppendLeft => build_append_feed_expr(right, left),
    }
}

fn build_pipe_feed_expr(source: Expr, sink: Expr) -> Expr {
    match sink {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(source),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_array_assign"),
                args: vec![source],
            }),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(source),
        },
        Expr::CodeVar(name) => Expr::AssignExpr {
            name: format!("&{}", name),
            expr: Box::new(source),
        },
        Expr::Index { target, index } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(source),
        },
        Expr::MultiDimIndex { target, dimensions } => Expr::IndexAssign {
            target,
            index: Box::new(Expr::ArrayLiteral(dimensions)),
            value: Box::new(source),
        },
        Expr::Call { name, mut args } => {
            args.push(source);
            Expr::Call { name, args }
        }
        Expr::CallOn { target, mut args } => {
            args.push(source);
            Expr::CallOn { target, args }
        }
        Expr::BareWord(name) => Expr::Call {
            name: Symbol::intern(&name),
            args: vec![source],
        },
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_whatever"),
            args: vec![source],
        },
        other => Expr::CallOn {
            target: Box::new(other),
            args: vec![source],
        },
    }
}

fn build_append_feed_expr(source: Expr, sink: Expr) -> Expr {
    match sink {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::Var(name), source],
            }),
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::ArrayVar(name), source],
            }),
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::Call {
                name: Symbol::intern("__mutsu_feed_append"),
                args: vec![Expr::HashVar(name), source],
            }),
        },
        Expr::Index { target, index } => {
            let current = Expr::Index {
                target: target.clone(),
                index: index.clone(),
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::Call {
                    name: Symbol::intern("__mutsu_feed_append"),
                    args: vec![current, source],
                }),
            }
        }
        Expr::Whatever => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append_whatever"),
            args: vec![source],
        },
        other => Expr::Call {
            name: Symbol::intern("__mutsu_feed_append"),
            args: vec![other, source],
        },
    }
}

pub(super) fn parse_custom_infix_word(input: &str) -> Option<(String, usize)> {
    let mut word_match: Option<(String, usize)> = None;

    // Try word-like operators (alphabetic/underscore start)
    let first = input.chars().next()?;
    if first.is_alphabetic() || first == '_' {
        let mut end = first.len_utf8();
        for ch in input[end..].chars() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                end += ch.len_utf8();
            } else {
                break;
            }
        }
        let name = &input[..end];
        if !is_reserved_infix_word(name) {
            word_match = Some((name.to_string(), end));
        }
    }

    // Try user-declared symbol/mixed operators (e.g. ©, ×, _<_)
    let symbol_match = super::super::stmt::simple::match_user_declared_infix_symbol_op(input);

    // Prefer the longer match
    match (word_match, symbol_match) {
        (Some(w), Some(s)) => {
            if s.1 >= w.1 {
                Some(s)
            } else {
                Some(w)
            }
        }
        (Some(w), None) => Some(w),
        (None, Some(s)) => Some(s),
        (None, None) => None,
    }
}

fn parse_flipflop_infix(input: &str) -> Option<(String, usize)> {
    const OPS: &[&str] = &["^fff^", "^fff", "fff^", "fff", "^ff^", "^ff", "ff^", "ff"];
    for op in OPS {
        if let Some(rest) = input.strip_prefix(op)
            && !is_ident_char(rest.as_bytes().first().copied())
        {
            return Some(((*op).to_string(), op.len()));
        }
    }
    None
}

fn is_reserved_infix_word(name: &str) -> bool {
    if name
        .chars()
        .next()
        .map(|c| c.is_ascii_uppercase())
        .unwrap_or(false)
    {
        return true;
    }
    if name.chars().all(|c| c.is_ascii_uppercase()) {
        return true;
    }
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "with"
            | "without"
            | "my"
            | "our"
            | "state"
            | "has"
            | "sub"
            | "method"
            | "class"
            | "role"
            | "grammar"
            | "module"
            | "package"
            | "token"
            | "rule"
            | "regex"
            | "multi"
            | "proto"
            | "constant"
            | "enum"
            | "subset"
            | "unit"
            | "use"
            | "need"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "fail"
            | "take"
            | "do"
            | "try"
            | "quietly"
            | "react"
            | "whenever"
            | "loop"
            | "repeat"
            | "let"
            | "temp"
            | "where"
            | "is"
            | "does"
            | "as"
            | "of"
            | "and"
            | "or"
            | "not"
            | "xor"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "min"
            | "max"
            | "cmp"
            | "leg"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "eqv"
            | "after"
            | "before"
            | "gcd"
            | "lcm"
            | "x"
            | "xx"
            | "o"
    )
}

/// Parse a comma-separated list of range_expr, returning (rest, items).
fn parse_comma_list_of_range_raw<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    let (r, first) = range_expr(input)
        .map_err(|err| enrich_expected_error(err, "expected expression", input.len()))?;
    let mut items = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
            break;
        }
        if let Ok((r3, next)) = range_expr(r2) {
            items.push(next);
            r = r3;
        } else {
            break;
        }
    }
    Ok((r, items))
}

/// Parse a comma-separated list of range_expr, returning (rest, Vec<Expr>).
fn parse_comma_list_of_range<'a>(input: &'a str) -> PResult<'a, Vec<Expr>> {
    parse_comma_list_of_range_raw(input)
}

/// Comparison: ==, !=, <, >, <=, >=, eq, ne, lt, gt, le, ge, ~~, !~~, ===, <=>
fn comparison_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    fn regex_rhs_needs_more_parsing(rest: &str) -> bool {
        let Ok((rest, _)) = ws(rest) else {
            return false;
        };
        parse_junctive_op(rest).is_some() || parse_junction_infix_op(rest).is_some()
    }

    let (rest, left) = junctive_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    // Detect Perl 5 =~ and !~ brainos (only when followed by space or m/)
    if r.starts_with("=~") && !r.starts_with("=~=") && !r.starts_with("=:=") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of =~ to do pattern matching; in Raku please use ~~"
                    .to_string(),
            ));
        }
    }
    if r.starts_with("!~") && !r.starts_with("!~~") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of !~ to do pattern matching; in Raku please use !~~"
                    .to_string(),
            ));
        }
    }
    // Detect !% which is an attempt to negate the non-boolean infix:<%;>
    if r.starts_with("!%") && !r.starts_with("!%%") && !is_ident_char(r.as_bytes().get(2).copied())
    {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate % because it is not iffy enough",
            r,
        ));
    }
    // Detect !+ !- !* !/ !~ (non-comparison) which cannot be negated
    if r.starts_with("!+") && !r.starts_with("!+&") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate + because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!-") && !r.starts_with("!--") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate - because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!*") && !r.starts_with("!**") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate * because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!/") && !r.starts_with("!//") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate / because it is not iffy enough",
            r,
        ));
    }
    // Detect !. which is not a valid metaop
    if r.starts_with("!.") && !r.starts_with("!..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Unsupported use of !. to concatenate strings; in Raku please use ~",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate . because it is too fiddly",
            r,
        ));
    }
    // Detect R. metaop misuse.
    if r.starts_with("R.") && !r.starts_with("R..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot reverse the args of . because it is too fiddly",
            r,
        ));
    }
    // Detect X. metaop misuse.
    if r.starts_with("X.") && !r.starts_with("X..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot do . because it is too fiddly",
            r,
        ));
    }
    if let Some((op, len)) = parse_negated_meta_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            junctive_expr_mode(r, mode).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected expression after comparison operator",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?
        } else {
            junctive_expr_mode(r, mode)?
        };
        let mut result = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right.clone()),
            }),
        };
        // Support chaining: "a" !after "b" !after "c" → (!after) && (!after)
        let mut prev_right = right;
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            // Try negated meta comparison for chaining
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained negated comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                let next_cmp = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(prev_right),
                        op: cop.token_kind(),
                        right: Box::new(next_right.clone()),
                    }),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
            // Also try regular comparison for mixed chaining
            if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                let next_cmp = Expr::Binary {
                    left: Box::new(prev_right),
                    op: cop.token_kind(),
                    right: Box::new(next_right.clone()),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
            break;
        }
        return Ok((r, result));
    }
    if let Some((op, len)) = parse_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, mut right) = if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch)
        {
            if let Ok((rest, expr)) = crate::parser::primary::regex::regex_lit(r) {
                if regex_rhs_needs_more_parsing(rest) {
                    if mode == ExprMode::Full {
                        junctive_expr_mode(r, mode).map_err(|err| {
                            // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                            if err.is_fatal() && err.exception.is_some() {
                                return err;
                            }
                            PError {
                                messages: merge_expected_messages(
                                    "expected expression after comparison operator",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r.len())),
                                exception: None,
                            }
                        })?
                    } else {
                        junctive_expr_mode(r, mode)?
                    }
                } else {
                    (rest, expr)
                }
            } else if mode == ExprMode::Full {
                junctive_expr_mode(r, mode).map_err(|err| {
                    // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                    if err.is_fatal() && err.exception.is_some() {
                        return err;
                    }
                    PError {
                        messages: merge_expected_messages(
                            "expected expression after comparison operator",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r.len())),
                        exception: None,
                    }
                })?
            } else {
                junctive_expr_mode(r, mode)?
            }
        } else if mode == ExprMode::Full {
            junctive_expr_mode(r, mode).map_err(|err| {
                // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                if err.is_fatal() && err.exception.is_some() {
                    return err;
                }
                PError {
                    messages: merge_expected_messages(
                        "expected expression after comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                    exception: None,
                }
            })?
        } else {
            junctive_expr_mode(r, mode)?
        };
        if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch) {
            right = wrap_smartmatch_rhs(right);
        }
        let mut operands = vec![left, right];
        let mut chain_ops: Vec<(TokenKind, bool)> = vec![(op.token_kind(), false)];
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained negated comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                chain_ops.push((cop.token_kind(), true));
                operands.push(next_right);
                r = r2;
                continue;
            }
            if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                let next_right =
                    if matches!(cop, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch) {
                        wrap_smartmatch_rhs(next_right)
                    } else {
                        next_right
                    };
                chain_ops.push((cop.token_kind(), false));
                operands.push(next_right);
                r = r2;
                continue;
            }
            break;
        }
        if chain_ops.len() == 1 {
            return Ok((
                r,
                make_chain_cmp(
                    operands[0].clone(),
                    chain_ops[0].0.clone(),
                    operands[1].clone(),
                    false,
                ),
            ));
        }
        let result = if operands.iter().any(contains_whatever) {
            // Keep the historical `lhs < * && * < rhs` shape so WhateverCode wrapping
            // can preserve placeholder semantics in expression arguments.
            build_chain_cmp_expr_with_repeated_middle(&operands, &chain_ops)
        } else {
            build_chain_cmp_expr(&operands, &chain_ops, 0, operands[0].clone())
        };
        return Ok((r, result));
    }

    Ok((rest, left))
}

fn make_chain_cmp(left: Expr, op: TokenKind, right: Expr, negated: bool) -> Expr {
    let cmp = Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };
    if negated {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cmp),
        }
    } else {
        cmp
    }
}

fn build_chain_cmp_expr(
    operands: &[Expr],
    ops: &[(TokenKind, bool)],
    index: usize,
    left: Expr,
) -> Expr {
    let (op, negated) = ops[index].clone();
    if index == ops.len() - 1 {
        return make_chain_cmp(left, op, operands[index + 1].clone(), negated);
    }

    let tmp_idx = CHAIN_CMP_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_name = format!("__mutsu_chain_cmp_{tmp_idx}");
    let tmp_var = Expr::Var(tmp_name.clone());
    let cmp = make_chain_cmp(left, op, tmp_var.clone(), negated);
    let rest = build_chain_cmp_expr(operands, ops, index + 1, tmp_var.clone());
    Expr::DoBlock {
        body: vec![
            Stmt::VarDecl {
                name: tmp_name,
                expr: operands[index + 1].clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::Binary {
                left: Box::new(cmp),
                op: TokenKind::AndAnd,
                right: Box::new(rest),
            }),
        ],
        label: None,
    }
}

fn build_chain_cmp_expr_with_repeated_middle(operands: &[Expr], ops: &[(TokenKind, bool)]) -> Expr {
    let mut result = make_chain_cmp(
        operands[0].clone(),
        ops[0].0.clone(),
        operands[1].clone(),
        ops[0].1,
    );
    let mut prev_right = operands[1].clone();
    for (idx, (op, negated)) in ops.iter().enumerate().skip(1) {
        let next_right = operands[idx + 1].clone();
        let next_cmp = make_chain_cmp(prev_right, op.clone(), next_right.clone(), *negated);
        result = Expr::Binary {
            left: Box::new(result),
            op: TokenKind::AndAnd,
            right: Box::new(next_cmp),
        };
        prev_right = next_right;
    }
    result
}

fn wrap_smartmatch_rhs(right: Expr) -> Expr {
    match right {
        // Keep Pair shape for `%hash ~~ key => !*.foo` by wrapping only value side.
        Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } => {
            let value = if contains_whatever(&right) && !matches!(&*right, Expr::Whatever) {
                wrap_whatevercode(&right)
            } else {
                *right
            };
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right: Box::new(value),
            }
        }
        other => {
            if contains_whatever(&other) && !matches!(&other, Expr::Whatever) {
                wrap_whatevercode(&other)
            } else {
                other
            }
        }
    }
}

/// Extract a comparison operator from the start of the input, returning the op and its length.
fn parse_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    // Unicode comparison operators
    if r.starts_with('\u{2A75}') {
        // ⩵ (U+2A75) — numeric equality (alias for ==)
        return Some((ComparisonOp::NumEq, '\u{2A75}'.len_utf8()));
    } else if r.starts_with('\u{2A76}') {
        // ⩶ (U+2A76) — value identity (alias for ===)
        return Some((ComparisonOp::StrictEq, '\u{2A76}'.len_utf8()));
    } else if r.starts_with('\u{2260}') {
        // ≠ (U+2260) — numeric inequality (alias for !=)
        return Some((ComparisonOp::NumNe, '\u{2260}'.len_utf8()));
    } else if r.starts_with('\u{2264}') {
        // ≤ (U+2264) — numeric less-than-or-equal (alias for <=)
        return Some((ComparisonOp::NumLe, '\u{2264}'.len_utf8()));
    } else if r.starts_with('\u{2265}') {
        // ≥ (U+2265) — numeric greater-than-or-equal (alias for >=)
        return Some((ComparisonOp::NumGe, '\u{2265}'.len_utf8()));
    }
    // ≅ (U+2245) — approximately equal
    if r.starts_with('\u{2245}') {
        return Some((ComparisonOp::ApproxEq, '\u{2245}'.len_utf8()));
    }
    if r.starts_with("=~=") {
        return Some((ComparisonOp::ApproxEq, 3));
    }
    if r.starts_with("!=:=") {
        return Some((ComparisonOp::ContainerNe, 4));
    }
    if r.starts_with("=:=") {
        return Some((ComparisonOp::ContainerEq, 3));
    }
    if r.starts_with("===") {
        Some((ComparisonOp::StrictEq, 3))
    } else if r.starts_with("==") && !r.starts_with("===") {
        Some((ComparisonOp::NumEq, 2))
    } else if r.starts_with("!%%") {
        Some((ComparisonOp::NotDivisibleBy, 3))
    } else if r.starts_with("!=") {
        Some((ComparisonOp::NumNe, 2))
    } else if r.starts_with("!~~") {
        Some((ComparisonOp::SmartNotMatch, 3))
    } else if r.starts_with("~~") {
        Some((ComparisonOp::SmartMatch, 2))
    } else if r.starts_with("<=>") {
        Some((ComparisonOp::Spaceship, 3))
    } else if r.starts_with("<=") && !r.starts_with("<=>") {
        Some((ComparisonOp::NumLe, 2))
    } else if r.starts_with(">=") {
        Some((ComparisonOp::NumGe, 2))
    } else if r.starts_with('<') && !r.starts_with("<<") && !r.starts_with("<=") {
        Some((ComparisonOp::NumLt, 1))
    } else if r.starts_with('>') && !r.starts_with(">>") && !r.starts_with(">=") {
        Some((ComparisonOp::NumGt, 1))
    } else if r.starts_with("eq") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrEq, 2))
    } else if r.starts_with("ne") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrNe, 2))
    } else if r.starts_with("lt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLt, 2))
    } else if r.starts_with("gt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGt, 2))
    } else if r.starts_with("le") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLe, 2))
    } else if r.starts_with("ge") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGe, 2))
    } else if r.starts_with("leg") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Leg, 3))
    } else if r.starts_with("cmp") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Cmp, 3))
    } else if r.starts_with("eqv") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Eqv, 3))
    } else if r.starts_with("before") && !is_ident_char(r.as_bytes().get(6).copied()) {
        Some((ComparisonOp::Before, 6))
    } else if r.starts_with("after") && !is_ident_char(r.as_bytes().get(5).copied()) {
        Some((ComparisonOp::After, 5))
    } else {
        None
    }
}

fn parse_negated_meta_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    let inner = r.strip_prefix('!')?;
    let (op, len) = parse_comparison_op(inner)?;
    // Operators that already have their own !-prefixed spelling are not meta-negated forms.
    if matches!(
        op,
        ComparisonOp::NumNe
            | ComparisonOp::NotDivisibleBy
            | ComparisonOp::SmartMatch
            | ComparisonOp::SmartNotMatch
    ) {
        return None;
    }
    Some((op, len + 1))
}

/// Range: ..  ..^  ^..  ^..^
pub(super) fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = structural_expr(input)?;
    let (r, _) = ws(rest)?;

    // Helper: wrap WhateverCode expressions in range endpoints.
    // `*-2` should become a WhateverCode lambda, but bare `*` should stay as Whatever.
    fn maybe_wrap_range_endpoint(expr: Expr) -> Expr {
        if contains_whatever(&expr) && !matches!(&expr, Expr::Whatever) {
            wrap_whatevercode(&expr)
        } else {
            expr
        }
    }

    if let Some(stripped) = r.strip_prefix("^..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected range RHS after '^..^'", r.len())
        })?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("^..") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '^..'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDot,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..^'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotCaret,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    if r.starts_with("..") && !r.starts_with("...") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..'", r.len()))?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDot,
                right: Box::new(maybe_wrap_range_endpoint(right)),
            },
        ));
    }
    Ok((rest, left))
}
