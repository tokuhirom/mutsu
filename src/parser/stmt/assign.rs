use super::super::expr::{expression, expression_no_sequence};
use super::super::helpers::ws;
use super::super::parse_result::{
    PError, PResult, merge_expected_messages, parse_char, take_while1,
};
use super::super::primary::parse_call_arg_list;

use crate::ast::{AssignOp, Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::{ident, parse_statement_modifier, var_name};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CompoundAssignOp {
    DefinedOr,
    LogicalOr,
    LogicalAnd,
    Add,
    Sub,
    Concat,
    Mul,
    Div,
    Mod,
    Power,
    Repeat,
    ListRepeat,
    BitOr,
    BitAnd,
    BitXor,
    Min,
    Max,
}

impl CompoundAssignOp {
    pub(super) fn symbol(self) -> &'static str {
        match self {
            CompoundAssignOp::DefinedOr => "//=",
            CompoundAssignOp::LogicalOr => "||=",
            CompoundAssignOp::LogicalAnd => "&&=",
            CompoundAssignOp::Add => "+=",
            CompoundAssignOp::Sub => "-=",
            CompoundAssignOp::Concat => "~=",
            CompoundAssignOp::Mul => "*=",
            CompoundAssignOp::Div => "/=",
            CompoundAssignOp::Mod => "%=",
            CompoundAssignOp::Power => "**=",
            CompoundAssignOp::Repeat => "x=",
            CompoundAssignOp::ListRepeat => "xx=",
            CompoundAssignOp::BitOr => "+|=",
            CompoundAssignOp::BitAnd => "+&=",
            CompoundAssignOp::BitXor => "+^=",
            CompoundAssignOp::Min => "min=",
            CompoundAssignOp::Max => "max=",
        }
    }

    pub(crate) fn token_kind(self) -> TokenKind {
        match self {
            CompoundAssignOp::DefinedOr => TokenKind::SlashSlash,
            CompoundAssignOp::LogicalOr => TokenKind::OrOr,
            CompoundAssignOp::LogicalAnd => TokenKind::AndAnd,
            CompoundAssignOp::Add => TokenKind::Plus,
            CompoundAssignOp::Sub => TokenKind::Minus,
            CompoundAssignOp::Concat => TokenKind::Tilde,
            CompoundAssignOp::Mul => TokenKind::Star,
            CompoundAssignOp::Div => TokenKind::Slash,
            CompoundAssignOp::Mod => TokenKind::Percent,
            CompoundAssignOp::Power => TokenKind::StarStar,
            CompoundAssignOp::Repeat => TokenKind::Ident("x".to_string()),
            CompoundAssignOp::ListRepeat => TokenKind::Ident("xx".to_string()),
            CompoundAssignOp::BitOr => TokenKind::BitOr,
            CompoundAssignOp::BitAnd => TokenKind::BitAnd,
            CompoundAssignOp::BitXor => TokenKind::BitXor,
            CompoundAssignOp::Min => TokenKind::Ident("min".to_string()),
            CompoundAssignOp::Max => TokenKind::Ident("max".to_string()),
        }
    }
}

pub(super) const COMPOUND_ASSIGN_OPS: &[CompoundAssignOp] = &[
    CompoundAssignOp::DefinedOr,
    CompoundAssignOp::LogicalOr,
    CompoundAssignOp::LogicalAnd,
    CompoundAssignOp::Power, // **= before *= to match longest first
    CompoundAssignOp::Add,
    CompoundAssignOp::Sub,
    CompoundAssignOp::Concat,
    CompoundAssignOp::Mul,
    CompoundAssignOp::Div,
    CompoundAssignOp::Mod,
    CompoundAssignOp::ListRepeat, // xx= before x= to match longest first
    CompoundAssignOp::Repeat,
    CompoundAssignOp::BitOr,
    CompoundAssignOp::BitAnd,
    CompoundAssignOp::BitXor,
    CompoundAssignOp::Min, // min= (word boundary checked in parse_compound_assign_op)
    CompoundAssignOp::Max, // max= (word boundary checked in parse_compound_assign_op)
];

pub(crate) fn parse_compound_assign_op(input: &str) -> Option<(&str, CompoundAssignOp)> {
    for op in COMPOUND_ASSIGN_OPS {
        if let Some(stripped) = input.strip_prefix(op.symbol()) {
            return Some((stripped, *op));
        }
    }
    None
}

fn parse_custom_compound_assign_op(input: &str) -> Option<(&str, String)> {
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if !first.is_alphabetic() && first != '_' {
        return None;
    }
    let mut end = first.len_utf8();
    for (idx, ch) in chars {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    let name = &input[..end];
    if matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "with"
            | "without"
            | "and"
            | "or"
            | "not"
    ) {
        return None;
    }
    let rest = &input[end..];
    if let Some(rest) = rest.strip_prefix('=') {
        return Some((rest, name.to_string()));
    }
    None
}

pub(super) fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
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

/// Try to parse a single assignment expression: $var op= expr or $var = expr.
/// Returns the expression as Expr::AssignExpr.
pub(super) fn try_parse_assign_expr(input: &str) -> PResult<'_, Expr> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    if sigil == b'$' && input.as_bytes().get(1).is_some_and(|c| *c == b'=') {
        return Err(PError::expected("assignment expression"));
    }
    if sigil != b'$' && sigil != b'@' && sigil != b'%' {
        return Err(PError::expected("assignment expression"));
    }
    let (r, var) = var_name(input)?;

    // Handle subscripted lvalues: @a[1] = ..., %h{key} = ..., $a[0] = ...
    if r.starts_with('[') || r.starts_with('{') || r.starts_with('<') {
        let closing = match r.as_bytes()[0] {
            b'[' => ']',
            b'{' => '}',
            _ => '>',
        };
        let (r_idx, _) = parse_char(r, r.as_bytes()[0] as char)?;
        let (r_idx, _) = ws(r_idx)?;
        let (r_idx, index_expr) = expression(r_idx)?;
        let (r_idx, _) = ws(r_idx)?;
        let (r_idx, _) = parse_char(r_idx, closing)?;
        let (r_after, _) = ws(r_idx)?;
        // Check for simple assignment
        if r_after.starts_with('=') && !r_after.starts_with("==") && !r_after.starts_with("=>") {
            let r3 = &r_after[1..];
            let (rest, _) = ws(r3)?;
            let (rest, rhs) = match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression(rest)?,
            };
            let target = match sigil {
                b'@' => Expr::ArrayVar(var.to_string()),
                b'%' => Expr::HashVar(var.to_string()),
                _ => Expr::Var(var.to_string()),
            };
            return Ok((
                rest,
                Expr::IndexAssign {
                    target: Box::new(target),
                    index: Box::new(index_expr),
                    value: Box::new(rhs),
                },
            ));
        }
        return Err(PError::expected("assignment expression"));
    }

    let (r2, _) = ws(r)?;
    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        _ => "",
    };
    // .= mutating method call: $var .= method(args) => $var = $var.method(args)
    if let Some(stripped) = r2.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        // Parse method name
        let (r, method_name) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let (r, _) = ws(r)?;
        // Parse optional args in parens
        let (rest, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                let (r, _) = parse_char(r, ')')?;
                (r, vec![])
            } else {
                let mut args = Vec::new();
                let (mut r, first) = expression_no_sequence(r)?;
                args.push(first);
                loop {
                    let (r2, _) = ws(r)?;
                    if !r2.starts_with(',') {
                        r = r2;
                        break;
                    }
                    let (r2, _) = ws(&r2[1..])?;
                    let (r2, next) = expression_no_sequence(r2)?;
                    args.push(next);
                    r = r2;
                }
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                (r, args)
            }
        } else {
            (r, vec![])
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::Var(var.to_string())),
                    name: method_name.to_string(),
                    args,
                    modifier: None,
                    quoted: false,
                }),
            },
        ));
    }
    if let Some((stripped, op)) = parse_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        // RHS: try chained assign, else single expression
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Var(var.to_string())),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                }),
            },
        ));
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(Expr::Var(name)),
                    right: vec![rhs],
                    modifier: None,
                }),
            },
        ));
    }
    // Check simple chained assignment: $var = ...
    if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
        let r3 = &r2[1..];
        let (rest, _) = ws(r3)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
            },
        ));
    }
    Err(PError::expected("assignment expression"))
}

/// Parse a comma expression (may produce a list).
pub(super) fn parse_comma_or_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, first) = expression(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with(',') && !r.starts_with(",,") {
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            return Ok((r, Expr::ArrayLiteral(vec![first])));
        }
        let mut items = vec![first];
        let (mut r, second) = expression(r)?;
        items.push(second);
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, next) = expression(r2)?;
            items.push(next);
            r = r2;
        }
    }
    Ok((rest, first))
}

/// If the list has exactly one item, return it directly instead of wrapping
/// in an ArrayLiteral.
fn finalize_list(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(items)
    }
}

/// In a comma-separated list, if the last item is a sequence expression
/// (Binary { ..., DotDotDot/DotDotDotCaret, ... }), merge all preceding
/// items into the sequence LHS.
/// E.g. `[0, 1, (*+* ... *)]` → `[ArrayLiteral([0, 1, *+*]) ... *]`
fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(op, TokenKind::DotDotDot | TokenKind::DotDotDotCaret)
    {
        // Merge preceding items + sequence LHS into a new ArrayLiteral
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

/// In a comma-separated list, if an item is a MetaOp (X+, Z-, etc.), merge
/// all preceding items into its left operand. This gives meta-ops effective
/// list-infix precedence: `1, 2 X+ 10` → `MetaOp(X, +, [1,2], 10)`.
pub(super) fn lift_meta_ops_in_list(items: Vec<Expr>) -> Vec<Expr> {
    // Find the first MetaOp in the list
    let meta_idx = items.iter().position(|e| matches!(e, Expr::MetaOp { .. }));
    if let Some(idx) = meta_idx
        && idx > 0
        && let Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } = &items[idx]
    {
        // Merge preceding items + meta's original left into a single list
        let mut seeds: Vec<Expr> = items[..idx].to_vec();
        seeds.push(*left.clone());
        let new_left = Expr::ArrayLiteral(seeds);
        let new_meta = Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(new_left),
            right: right.clone(),
        };
        let mut result = vec![new_meta];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}

pub(super) fn assign_stmt(input: &str) -> PResult<'_, Stmt> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    let is_sigiled = sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&';

    // Try bare identifier assignment for sigilless variables: a = expr
    if !is_sigiled {
        if let Ok((after_ident, bare_name)) = ident(input) {
            let (after_ws, _) = ws(after_ident)?;
            if after_ws.starts_with('=') && !after_ws.starts_with("==") {
                let rest = &after_ws[1..];
                let (rest, _) = ws(rest)?;
                let (rest, expr) = parse_assign_expr_or_comma(rest)?;
                let stmt = Stmt::Assign {
                    name: bare_name,
                    expr,
                    op: AssignOp::Assign,
                };
                return parse_statement_modifier(rest, stmt);
            }
        }
        return Err(PError::expected("assignment"));
    }

    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        b'&' => "&",
        _ => "",
    };

    let (rest, var) = var_name(input)?;
    let name = format!("{}{}", prefix, var);
    let (rest, _) = ws(rest)?;
    let var_expr = if sigil == b'@' {
        Expr::ArrayVar(var.clone())
    } else if sigil == b'%' {
        Expr::HashVar(var.clone())
    } else {
        Expr::Var(name.clone())
    };

    // Mutating method compound assignment: $x.m += expr
    if let Some(after_dot) = rest.strip_prefix('.')
        && !after_dot.starts_with('=')
    {
        let (after_name, method_name) = take_while1(after_dot, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let (after_name, _) = ws(after_name)?;
        if let Some((after_op, op)) = parse_compound_assign_op(after_name) {
            let (after_op, _) = ws(after_op)?;
            let (rest, rhs) = parse_assign_expr_or_comma(after_op)?;
            let current_value = Expr::MethodCall {
                target: Box::new(var_expr.clone()),
                name: method_name.clone(),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            let updated_value = Expr::Binary {
                left: Box::new(current_value),
                op: op.token_kind(),
                right: Box::new(rhs),
            };
            let assign_call = Expr::Call {
                name: "__mutsu_assign_method_lvalue".to_string(),
                args: vec![
                    var_expr,
                    Expr::Literal(Value::Str(method_name)),
                    Expr::ArrayLiteral(Vec::new()),
                    updated_value,
                    Expr::Literal(Value::Str(name.clone())),
                ],
            };
            let stmt = Stmt::Expr(assign_call);
            return parse_statement_modifier(rest, stmt);
        }
    }

    // Meta-op assignment: @a [X+]= @b → @a = @a X+ @b
    if let Some(rest_after_bracket) = rest.strip_prefix('[')
        && let Some(bracket_end) = rest_after_bracket.find(']')
    {
        let meta_op_str = &rest_after_bracket[..bracket_end];
        let after_bracket = &rest_after_bracket[bracket_end + 1..];
        if after_bracket.starts_with('=') && !after_bracket.starts_with("==") {
            let after_eq = &after_bracket[1..];
            let (after_eq, _) = ws(after_eq)?;
            let (rest, rhs) = parse_assign_expr_or_comma(after_eq)?;
            // Determine meta and op
            let (meta, op) = if let Some(op_str) = meta_op_str.strip_prefix('R') {
                ("R", op_str)
            } else if let Some(op_str) = meta_op_str.strip_prefix('X') {
                ("X", op_str)
            } else if let Some(op_str) = meta_op_str.strip_prefix('Z') {
                ("Z", op_str)
            } else {
                return Err(PError::expected("meta operator (R/X/Z) in [op]="));
            };
            let expr = Expr::MetaOp {
                meta: meta.to_string(),
                op: op.to_string(),
                left: Box::new(var_expr),
                right: Box::new(rhs),
            };
            let stmt = Stmt::Assign {
                name,
                expr,
                op: AssignOp::Assign,
            };
            return parse_statement_modifier(rest, stmt);
        }
    }

    if let Some((stripped, op)) = parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        let expr = Expr::Binary {
            left: Box::new(Expr::Var(name.clone())),
            op: op.token_kind(),
            right: Box::new(rhs),
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after compound assignment",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        let stmt = Stmt::Assign {
            name: name.clone(),
            expr: Expr::InfixFunc {
                name: op_name,
                left: Box::new(var_expr),
                right: vec![rhs],
                modifier: None,
            },
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }

    // Mutating method call: $x.=method or $x .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })
        .map_err(|err| PError {
            messages: merge_expected_messages("expected method name after '.='", &err.messages),
            remaining_len: err.remaining_len.or(Some(stripped.len())),
        })?;
        let method_name = method_name.to_string();
        let (r, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, a) = parse_call_arg_list(r).map_err(|err| PError {
                messages: merge_expected_messages("expected method call arguments", &err.messages),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, a)
        } else {
            (r, Vec::new())
        };
        let var_expr = if sigil == b'@' {
            Expr::ArrayVar(var.clone())
        } else if sigil == b'%' {
            Expr::HashVar(var.clone())
        } else {
            Expr::Var(var.clone())
        };
        let expr = Expr::MethodCall {
            target: Box::new(var_expr),
            name: method_name,
            args,
            modifier: None,
            quoted: false,
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(r, stmt);
    }

    // Detect Perl 5 =~ braino: only when followed by space or m/ (not =~$var which is = ~$var)
    if rest.starts_with("=~") && !rest.starts_with("=~=") && !rest.starts_with("=:=") {
        let after = &rest[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::expected_at(
                "Unsupported use of =~ to do pattern matching; in Raku please use ~~",
                rest,
            ));
        }
    }
    // Simple assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after '='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    // Binding (:= or ::=)
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_comma_or_expr(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected right-hand expression after ':='",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
        })?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Bind,
        };
        return parse_statement_modifier(rest, stmt);
    }

    Err(PError::expected("assignment"))
}
