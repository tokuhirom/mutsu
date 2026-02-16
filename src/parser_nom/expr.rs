use super::parse_result::{PResult, parse_char, parse_tag, take_while1};

use crate::ast::{Expr, Stmt};
use crate::lexer::TokenKind;
use crate::value::Value;

use super::helpers::ws;
use super::primary::{parse_call_arg_list, primary};

/// Parse an expression (full precedence).
pub(super) fn expression(input: &str) -> PResult<'_, Expr> {
    let (rest, mut expr) = ternary(input)?;
    // Handle => (fat arrow / pair constructor) - lower precedence than ternary
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = or_expr(r)?;
        // Auto-quote bareword on LHS of => to a string literal
        let left = match expr {
            Expr::BareWord(ref name) => Expr::Literal(Value::Str(name.clone())),
            _ => expr,
        };
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            },
        ));
    }
    // Wrap WhateverCode expressions in a lambda
    if contains_whatever(&expr) {
        let body_expr = replace_whatever(&expr);
        expr = Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
        };
    }
    Ok((rest, expr))
}

fn is_whatever(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Value::Num(f)) if f.is_infinite() && f.is_sign_positive())
}

fn contains_whatever(expr: &Expr) -> bool {
    match expr {
        e if is_whatever(e) => true,
        Expr::Binary { left, right, .. } => contains_whatever(left) || contains_whatever(right),
        Expr::Unary { expr, .. } => contains_whatever(expr),
        _ => false,
    }
}

fn replace_whatever(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever(left)),
            op: op.clone(),
            right: Box::new(replace_whatever(right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever(expr)),
        },
        _ => expr.clone(),
    }
}

/// Ternary: expr ?? expr !! expr
fn ternary(input: &str) -> PResult<'_, Expr> {
    let (input, cond) = or_expr(input)?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_tag(input, "??") {
        let (input, _) = ws(input)?;
        let (input, then_expr) = expression(input)?;
        let (input, _) = ws(input)?;
        let (input, _) = parse_tag(input, "!!")?;
        let (input, _) = ws(input)?;
        let (input, else_expr) = expression(input)?;
        return Ok((
            input,
            Expr::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ));
    }
    Ok((input, cond))
}

/// Low-precedence: or / and / not
fn or_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        // `or`
        if r.starts_with("or") && !is_ident_char(r.as_bytes().get(2).copied()) {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, right) = and_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::OrWord,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn and_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with("and") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = not_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::AndAnd,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

fn not_expr(input: &str) -> PResult<'_, Expr> {
    if input.starts_with("not") && !is_ident_char(input.as_bytes().get(3).copied()) {
        let r = &input[3..];
        let (r, _) = ws(r)?;
        let (r, expr) = not_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    or_or_expr(input)
}

/// || and //
fn or_or_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some(stripped) = r.strip_prefix("||") {
            let (r, _) = ws(stripped)?;
            let (r, right) = and_and_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::OrOr,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("//") && !r.starts_with("///") {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, right) = and_and_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::SlashSlash,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// &&
fn and_and_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if let Some(stripped) = r.strip_prefix("&&") {
            let (r, _) = ws(stripped)?;
            let (r, right) = comparison_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::AndAnd,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Comparison: ==, !=, <, >, <=, >=, eq, ne, lt, gt, le, ge, ~~, !~~, ===, <=>
fn comparison_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = range_expr(input)?;
    let (r, _) = ws(rest)?;

    // Try multi-char operators first
    let (op, len): (Option<TokenKind>, usize) = if r.starts_with("===") {
        (Some(TokenKind::EqEqEq), 3)
    } else if r.starts_with("==") && !r.starts_with("===") {
        (Some(TokenKind::EqEq), 2)
    } else if r.starts_with("!=") {
        (Some(TokenKind::BangEq), 2)
    } else if r.starts_with("!~~") {
        (Some(TokenKind::BangTilde), 3)
    } else if r.starts_with("~~") {
        (Some(TokenKind::SmartMatch), 2)
    } else if r.starts_with("<=>") {
        (Some(TokenKind::LtEqGt), 3)
    } else if r.starts_with("<=") && !r.starts_with("<=>") {
        (Some(TokenKind::Lte), 2)
    } else if r.starts_with(">=") {
        (Some(TokenKind::Gte), 2)
    } else if r.starts_with('<') && !r.starts_with("<<") && !r.starts_with("<=") {
        (Some(TokenKind::Lt), 1)
    } else if r.starts_with('>') && !r.starts_with(">>") && !r.starts_with(">=") {
        (Some(TokenKind::Gt), 1)
    } else if r.starts_with("eq") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("eq".to_string())), 2)
    } else if r.starts_with("ne") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("ne".to_string())), 2)
    } else if r.starts_with("lt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("lt".to_string())), 2)
    } else if r.starts_with("gt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("gt".to_string())), 2)
    } else if r.starts_with("le") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("le".to_string())), 2)
    } else if r.starts_with("ge") && !is_ident_char(r.as_bytes().get(2).copied()) {
        (Some(TokenKind::Ident("ge".to_string())), 2)
    } else if r.starts_with("leg") && !is_ident_char(r.as_bytes().get(3).copied()) {
        (Some(TokenKind::Ident("leg".to_string())), 3)
    } else if r.starts_with("cmp") && !is_ident_char(r.as_bytes().get(3).copied()) {
        (Some(TokenKind::Ident("cmp".to_string())), 3)
    } else {
        (None, 0)
    };

    if let Some(op) = op {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = range_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
        ));
    }

    Ok((rest, left))
}

/// Range: ..  ..^  ^..  ^..^
fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = concat_expr(input)?;
    let (r, _) = ws(rest)?;

    if let Some(stripped) = r.strip_prefix("^..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = concat_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("^..") {
        let (r, _) = ws(stripped)?;
        let (r, right) = concat_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDot,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("..^") {
        let (r, _) = ws(stripped)?;
        let (r, right) = concat_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if r.starts_with("..") && !r.starts_with("...") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, right) = concat_expr(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDot,
                right: Box::new(right),
            },
        ));
    }
    Ok((rest, left))
}

/// String concatenation: ~
fn concat_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = additive_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with('~') && !r.starts_with("~~") && !r.starts_with("~=") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Tilde,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // `x` (string replicate)
        if r.starts_with('x') && !is_ident_char(r.as_bytes().get(1).copied()) {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = additive_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("x".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Addition/subtraction: + -
fn additive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = multiplicative_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with('+') && !r.starts_with("++") && !r.starts_with("+=") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Plus,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with('-')
            && !r.starts_with("--")
            && !r.starts_with("-=")
            && !r.starts_with("->")
        {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Minus,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Multiplication/division: * / % div mod gcd lcm
fn multiplicative_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = power_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with('*') && !r.starts_with("**") && !r.starts_with("*=") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Star,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with('/') && !r.starts_with("//") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Slash,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with('%')
            && !r.starts_with("%%")
            && !is_ident_char(r.as_bytes().get(1).copied())
        {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Percent,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if let Some(stripped) = r.strip_prefix("%%") {
            let (r, _) = ws(stripped)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::PercentPercent,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("div") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("div".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("mod") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("mod".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("gcd") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("gcd".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if r.starts_with("lcm") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
            let (r, _) = ws(r)?;
            let (r, right) = power_expr(r)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("lcm".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Exponentiation: **
fn power_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, base) = prefix_expr(input)?;
    let (r, _) = ws(rest)?;
    if let Some(stripped) = r.strip_prefix("**") {
        let (r, _) = ws(stripped)?;
        let (r, exp) = power_expr(r)?; // right-associative
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(base),
                op: TokenKind::StarStar,
                right: Box::new(exp),
            },
        ));
    }
    Ok((rest, base))
}

/// Prefix unary: !, ?, +, -, ~, so, not, ++, --
fn prefix_expr(input: &str) -> PResult<'_, Expr> {
    if input.starts_with('!') && !input.starts_with("!!") && !input.starts_with("!~~") {
        let r = &input[1..];
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    if input.starts_with('?') && !input.starts_with("??") {
        let r = &input[1..];
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    if input.starts_with("so") && !is_ident_char(input.as_bytes().get(2).copied()) {
        let r = &input[2..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    if let Some(stripped) = input.strip_prefix("++") {
        let (r, expr) = postfix_expr(stripped)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::PlusPlus,
                expr: Box::new(expr),
            },
        ));
    }
    if let Some(stripped) = input.strip_prefix("--") {
        let (r, expr) = postfix_expr(stripped)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::MinusMinus,
                expr: Box::new(expr),
            },
        ));
    }
    // Unary minus: only if followed by non-space primary (e.g., -42, -$x)
    if input.starts_with('-')
        && !input.starts_with("--")
        && !input.starts_with("->")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'@' || c == b'(' || c.is_ascii_digit())
    {
        let r = &input[1..];
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Minus,
                expr: Box::new(expr),
            },
        ));
    }
    // Unary + (numeric coercion)
    if input.starts_with('+')
        && !input.starts_with("++")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'@' || c == b'(')
    {
        let r = &input[1..];
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Plus,
                expr: Box::new(expr),
            },
        ));
    }
    // Unary ~ (string coercion)
    if input.starts_with('~')
        && !input.starts_with("~~")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'@' || c == b'(' || c == b'"' || c == b'\'')
    {
        let r = &input[1..];
        let (r, expr) = prefix_expr(r)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Tilde,
                expr: Box::new(expr),
            },
        ));
    }
    postfix_expr(input)
}

/// Postfix: method calls (.method), indexing ([]), ++, --
fn postfix_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut expr) = primary(input)?;

    loop {
        // Method call: .method or .method(args) or .method: args
        // Also handles modifiers: .?method, .!method
        // Also handles: .^method (meta-method)
        if rest.starts_with('.') && !rest.starts_with("..") {
            let r = &rest[1..];
            // Check for modifier
            let (r, modifier) = if let Some(stripped) = r.strip_prefix('?') {
                (stripped, Some('?'))
            } else if let Some(stripped) = r.strip_prefix('^') {
                (stripped, Some('^'))
            } else {
                (r, None)
            };
            // Parse method name
            if let Ok((r, name)) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                let name = name.to_string();
                // Check for args in parens
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, args) = parse_call_arg_list(r)?;
                    let (r, _) = ws(r)?;
                    let (r, _) = parse_char(r, ')')?;
                    expr = Expr::MethodCall {
                        target: Box::new(expr),
                        name,
                        args,
                        modifier,
                    };
                    rest = r;
                    continue;
                }
                // Check for colon-arg syntax: .method: arg, arg2
                let (r2, _) = ws(r)?;
                if r2.starts_with(':') && !r2.starts_with("::") {
                    let r3 = &r2[1..];
                    let (r3, _) = ws(r3)?;
                    // Parse the arguments after colon
                    if let Ok((r3, first_arg)) = expression(r3) {
                        let mut args = vec![first_arg];
                        let mut r_inner = r3;
                        loop {
                            let (r4, _) = ws(r_inner)?;
                            if !r4.starts_with(',') {
                                break;
                            }
                            let r4 = &r4[1..];
                            let (r4, _) = ws(r4)?;
                            if let Ok((r4, next)) = expression(r4) {
                                args.push(next);
                                r_inner = r4;
                            } else {
                                break;
                            }
                        }
                        expr = Expr::MethodCall {
                            target: Box::new(expr),
                            name,
                            args,
                            modifier,
                        };
                        rest = r_inner;
                        continue;
                    }
                }
                // No-arg method call
                expr = Expr::MethodCall {
                    target: Box::new(expr),
                    name,
                    args: Vec::new(),
                    modifier,
                };
                rest = r;
                continue;
            }
        }

        // Array indexing: [expr]
        if rest.starts_with('[') {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Hash indexing with angle brackets: %hash<key> or $hash<key>
        if rest.starts_with('<') && !rest.starts_with("<=") && !rest.starts_with("<<") {
            // Only for hash/var targets
            if matches!(&expr, Expr::HashVar(_) | Expr::Var(_)) {
                let r = &rest[1..];
                if let Some(end) = r.find('>') {
                    let key = &r[..end];
                    let r = &r[end + 1..];
                    expr = Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(Expr::Literal(Value::Str(key.to_string()))),
                    };
                    rest = r;
                    continue;
                }
            }
        }

        // Hash indexing with braces: %hash{"key"} or %hash{$var}
        if rest.starts_with('{') && matches!(&expr, Expr::HashVar(_) | Expr::Var(_)) {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Postfix ++ and --
        if let Some(stripped) = rest.strip_prefix("++") {
            rest = stripped;
            expr = Expr::PostfixOp {
                op: TokenKind::PlusPlus,
                expr: Box::new(expr),
            };
            continue;
        }
        if let Some(stripped) = rest.strip_prefix("--") {
            rest = stripped;
            expr = Expr::PostfixOp {
                op: TokenKind::MinusMinus,
                expr: Box::new(expr),
            };
            continue;
        }

        break;
    }

    Ok((rest, expr))
}

/// Check if a byte is a valid identifier continuation character.
fn is_ident_char(b: Option<u8>) -> bool {
    match b {
        Some(c) => c.is_ascii_alphanumeric() || c == b'_' || c == b'-',
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_binary_add() {
        let (rest, expr) = expression("1 + 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::Plus,
                ..
            }
        ));
    }

    #[test]
    fn parse_comparison() {
        let (rest, expr) = expression("$x == $y").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::EqEq,
                ..
            }
        ));
    }

    #[test]
    fn parse_method_call() {
        let (rest, expr) = expression("$x.defined").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::MethodCall { .. }));
    }

    #[test]
    fn parse_ternary() {
        let (rest, expr) = expression("$x ?? 1 !! 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Ternary { .. }));
    }
}
