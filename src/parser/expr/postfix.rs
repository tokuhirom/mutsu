use super::super::helpers::{is_ident_char, ws};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use super::super::primary::{parse_call_arg_list, primary};

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::expression;
use super::operators::{parse_postfix_update_op, parse_prefix_unary_op};

pub(super) fn prefix_expr(input: &str) -> PResult<'_, Expr> {
    if let Some((op, len)) = parse_prefix_unary_op(input) {
        let mut rest = &input[len..];
        if op.consumes_ws() {
            let (r, _) = ws(rest)?;
            rest = r;
        }
        let (rest, expr) = if op.parses_postfix_target() {
            postfix_expr(rest)?
        } else {
            prefix_expr(rest)?
        };
        return Ok((
            rest,
            Expr::Unary {
                op: op.token_kind(),
                expr: Box::new(expr),
            },
        ));
    }
    // not(expr) — tight-binding form: not followed by ( without space
    if input.starts_with("not(") {
        let r = &input[3..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    // so(expr) — tight-binding form: so followed by ( without space
    if input.starts_with("so(") {
        let r = &input[2..];
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    // lazy prefix (treated as no-op): lazy expr
    if input.starts_with("lazy") && !is_ident_char(input.as_bytes().get(4).copied()) {
        let r = &input[4..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((r, expr));
    }
    // eager prefix (treated as no-op): eager expr
    if input.starts_with("eager") && !is_ident_char(input.as_bytes().get(5).copied()) {
        let r = &input[5..];
        let (r, _) = ws(r)?;
        let (r, expr) = prefix_expr(r)?;
        return Ok((r, expr));
    }
    // ^expr — upto operator: ^5 means 0..^5
    if input.starts_with('^')
        && !input.starts_with("^..")
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'$' || c == b'(' || c.is_ascii_digit() || c.is_ascii_alphabetic() || c == b'_')
    {
        let rest = &input[1..];
        let (rest, expr) = postfix_expr(rest)?;
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Int(0))),
                op: TokenKind::DotDotCaret,
                right: Box::new(expr),
            },
        ));
    }
    // |@array or |%hash or |$scalar — slip/flatten prefix
    if input.starts_with('|')
        && let Some(&c) = input.as_bytes().get(1)
        && (c == b'@' || c == b'%' || c == b'$')
    {
        let rest = &input[1..];
        let (rest, expr) = postfix_expr(rest)?;
        return Ok((
            rest,
            Expr::Unary {
                op: TokenKind::Pipe,
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
        // Unspace: backslash + whitespace collapses to nothing, allowing
        // `foo\ .method` to parse as `foo.method`.
        if rest.starts_with('\\') {
            let after_bs = &rest[1..];
            if let Some(c) = after_bs.chars().next()
                && c.is_whitespace()
            {
                // Peek ahead: consume unspace only if followed by a postfix operator
                let mut scan = &after_bs[c.len_utf8()..];
                while let Some(c2) = scan.chars().next() {
                    if c2.is_whitespace() {
                        scan = &scan[c2.len_utf8()..];
                    } else {
                        break;
                    }
                }
                if scan.starts_with('.') && !scan.starts_with("..") {
                    rest = scan;
                }
            }
        }
        // Method call: .method or .method(args) or .method: args
        // Also handles modifiers: .?method, .!method
        // Also handles: .^method (meta-method)
        // Also handles call-on: .(args)
        if rest.starts_with('.') && !rest.starts_with("..") {
            let r = &rest[1..];
            // Check for .[index] syntax: object.[expr]
            if let Some(r) = r.strip_prefix('[') {
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
            // Check for call-on syntax: .(args)
            if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                expr = Expr::CallOn {
                    target: Box::new(expr),
                    args,
                };
                rest = r;
                continue;
            }
            // Check for modifier: .?method, .^method, .+method, .*method
            let (r, modifier) = if let Some(stripped) = r.strip_prefix('?') {
                (stripped, Some('?'))
            } else if let Some(stripped) = r.strip_prefix('^') {
                (stripped, Some('^'))
            } else if let Some(stripped) = r.strip_prefix('+') {
                (stripped, Some('+'))
            } else if r.starts_with('*') && !r.starts_with("**") {
                (&r[1..], Some('*'))
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
                    let (r3, first_arg) = expression(r3)?;
                    let mut args = vec![first_arg];
                    let mut r_inner = r3;
                    loop {
                        let (r4, _) = ws(r_inner)?;
                        if !r4.starts_with(',') {
                            break;
                        }
                        let r4 = &r4[1..];
                        let (r4, _) = ws(r4)?;
                        let (r4, next) = expression(r4)?;
                        args.push(next);
                        r_inner = r4;
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
            return Err(PError::expected_at("method name", r));
        }

        // CallOn: $var(args) — invoke a callable stored in a variable
        if rest.starts_with('(')
            && matches!(
                &expr,
                Expr::Var(_)
                    | Expr::CodeVar(_)
                    | Expr::MethodCall { .. }
                    | Expr::AnonSub(_)
                    | Expr::AnonSubParams { .. }
                    | Expr::Lambda { .. }
                    | Expr::Index { .. }
                    | Expr::CallOn { .. }
            )
        {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            expr = Expr::CallOn {
                target: Box::new(expr),
                args,
            };
            rest = r;
            continue;
        }

        // Array indexing: [expr] or zen slice []
        if rest.starts_with('[') {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            // Zen slice: expr[] — returns expr unchanged (identity on lists)
            if let Some(after) = r.strip_prefix(']') {
                rest = after;
                continue;
            }
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

        // Hash indexing with angle brackets: %hash<key>, $hash<key>, @a[0]<key>, etc.
        // Only match when content is a simple word key (alphanumeric, no operators)
        if rest.starts_with('<')
            && !rest.starts_with("<=")
            && !rest.starts_with("<<")
            && !rest.starts_with("<=>")
        {
            let r = &rest[1..];
            let Some(end) = r.find('>') else {
                return Err(PError::expected_at("closing '>'", r));
            };
            let key = &r[..end];
            if key.is_empty()
                || !key
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
            {
                return Err(PError::expected_at("angle index key", r));
            }
            let r = &r[end + 1..];
            // Check for :exists / :delete adverbs
            if r.starts_with(":exists") && !is_ident_char(r.as_bytes().get(7).copied()) {
                let r = &r[7..];
                expr = Expr::Exists(Box::new(Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(Expr::Literal(Value::Str(key.to_string()))),
                }));
                rest = r;
                continue;
            }
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(Expr::Literal(Value::Str(key.to_string()))),
            };
            rest = r;
            continue;
        }

        // Hash indexing with braces: %hash{"key"}, %hash{$var}, @a[0]{"key"}, etc.
        if rest.starts_with('{')
            && matches!(&expr, Expr::HashVar(_) | Expr::Var(_) | Expr::Index { .. })
        {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, index) = expression(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, '}')?;
            // Check for :exists / :delete adverbs on curly-brace subscript
            if r.starts_with(":exists") && !is_ident_char(r.as_bytes().get(7).copied()) {
                let r = &r[7..];
                expr = Expr::Exists(Box::new(Expr::Index {
                    target: Box::new(expr),
                    index: Box::new(index),
                }));
                rest = r;
                continue;
            }
            if r.starts_with(":delete") && !is_ident_char(r.as_bytes().get(7).copied()) {
                let r = &r[7..];
                // Use MethodCall as a proxy for delete operation
                expr = Expr::MethodCall {
                    target: Box::new(Expr::Index {
                        target: Box::new(expr),
                        index: Box::new(index),
                    }),
                    name: "DELETE-KEY".to_string(),
                    args: vec![],
                    modifier: None,
                };
                rest = r;
                continue;
            }
            expr = Expr::Index {
                target: Box::new(expr),
                index: Box::new(index),
            };
            rest = r;
            continue;
        }

        // Postfix ++ and --
        if let Some((op, len)) = parse_postfix_update_op(rest) {
            rest = &rest[len..];
            expr = Expr::PostfixOp {
                op: op.token_kind(),
                expr: Box::new(expr),
            };
            continue;
        }

        // Postfix i (imaginary number): (expr)i → Complex(0, expr)
        if rest.starts_with('i') && !is_ident_char(rest.as_bytes().get(1).copied()) {
            rest = &rest[1..];
            expr = Expr::MethodCall {
                target: Box::new(expr),
                name: "i".to_string(),
                args: vec![],
                modifier: None,
            };
            continue;
        }

        // Superscript digits as postfix exponentiation: x² → x ** 2, x³ → x ** 3
        if let Some((exp, len)) = parse_superscript_exp(rest) {
            rest = &rest[len..];
            expr = Expr::Binary {
                left: Box::new(expr),
                op: crate::token_kind::TokenKind::StarStar,
                right: Box::new(Expr::Literal(Value::Int(exp))),
            };
            continue;
        }

        break;
    }

    Ok((rest, expr))
}

/// Parse superscript digits as an exponent. Returns (exponent_value, byte_length).
fn parse_superscript_exp(input: &str) -> Option<(i64, usize)> {
    fn superscript_digit(c: char) -> Option<i64> {
        match c {
            '\u{2070}' => Some(0), // ⁰
            '\u{00B9}' => Some(1), // ¹
            '\u{00B2}' => Some(2), // ²
            '\u{00B3}' => Some(3), // ³
            '\u{2074}' => Some(4), // ⁴
            '\u{2075}' => Some(5), // ⁵
            '\u{2076}' => Some(6), // ⁶
            '\u{2077}' => Some(7), // ⁷
            '\u{2078}' => Some(8), // ⁸
            '\u{2079}' => Some(9), // ⁹
            _ => None,
        }
    }
    let mut chars = input.chars();
    let first = chars.next()?;
    let mut value = superscript_digit(first)?;
    let mut len = first.len_utf8();
    for c in chars {
        if let Some(d) = superscript_digit(c) {
            value = value * 10 + d;
            len += c.len_utf8();
        } else {
            break;
        }
    }
    Some((value, len))
}
