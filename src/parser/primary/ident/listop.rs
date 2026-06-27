use crate::ast::Expr;
use crate::parser::expr::{call_arg_expr, expression, expression_no_sequence};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, merge_expected_messages};
use crate::parser::primary::current_line_number;
use crate::parser::primary::ident::predicates::is_stmt_modifier_ahead;
use crate::symbol::Symbol;
use crate::value::Value;

pub(crate) const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";
pub(crate) const CALLFRAME_LINE_KEY: &str = "__callframe_line";

pub(crate) fn attach_test_callsite_line(name: &str, input: &str, mut args: Vec<Expr>) -> Vec<Expr> {
    if crate::parser::stmt::simple::is_test_assertion_callable(name) {
        args.push(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(
                TEST_CALLSITE_LINE_KEY.to_string(),
            ))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Int(current_line_number(input)))),
        });
    }
    if name == "callframe" || name == "caller" {
        args.push(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(CALLFRAME_LINE_KEY.to_string()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Int(current_line_number(input)))),
        });
    }
    args
}

pub(crate) fn make_call_expr(name: String, input: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(&name),
        args: attach_test_callsite_line(&name, input, args),
    }
}

/// Parse expression listop arguments: comma-separated full expressions.
/// Stops at statement modifiers, semicolons, and closing brackets.
pub(crate) fn parse_expr_listop_args(input: &str, name: String) -> PResult<'_, Expr> {
    if name == "make" {
        // Use expression_no_sequence so that `make X => Y` parses the entire
        // Pair as the argument (fat-arrow has lower precedence than or_expr).
        let (r, arg) = expression_no_sequence(input).map_err(|err| PError {
            messages: merge_expected_messages("expected listop argument expression", &err.messages),
            remaining_len: err.remaining_len.or(Some(input.len())),
            exception: None,
        })?;
        return Ok((r, make_call_expr(name, input, vec![arg])));
    }

    // Raku listop `slip ...` takes a single expression argument, which may
    // itself be a comma expression (e.g. `slip (2,3), 4`).
    if name == "slip" {
        let (r, first) = expression(input).map_err(|err| PError {
            messages: merge_expected_messages("expected listop argument expression", &err.messages),
            remaining_len: err.remaining_len.or(Some(input.len())),
            exception: None,
        })?;
        let mut exprs = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') || r2.starts_with(",,") {
                break;
            }
            let r2 = &r2[1..];
            let (r2, _) = ws(r2)?;
            if r2.is_empty()
                || r2.starts_with(';')
                || r2.starts_with('}')
                || r2.starts_with(')')
                || is_stmt_modifier_ahead(r2)
            {
                break;
            }
            let (r2, expr) = expression(r2).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected listop argument expression after ','",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r2.len())),
                exception: None,
            })?;
            exprs.push(expr);
            r = r2;
        }
        let arg = if exprs.len() == 1 {
            exprs.remove(0)
        } else {
            Expr::ArrayLiteral(exprs)
        };
        return Ok((r, make_call_expr(name, input, vec![arg])));
    }

    let (r, first) = expression(input).map_err(|err| PError {
        messages: merge_expected_messages("expected listop argument expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
        exception: None,
    })?;
    let (r, invocant_colon_call) = try_parse_no_paren_invocant_colon_call(&name, first.clone(), r)?;
    if let Some(method_call) = invocant_colon_call {
        return Ok((r, method_call));
    }
    let mut args = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        // Stop at terminators
        if r2.is_empty()
            || r2.starts_with(';')
            || r2.starts_with('}')
            || r2.starts_with(')')
            || is_stmt_modifier_ahead(r2)
        {
            break;
        }
        let (r2, arg) = expression(r2).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected listop argument expression after ','",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r2.len())),
            exception: None,
        })?;
        args.push(arg);
        r = r2;
    }
    Ok((r, make_call_expr(name, input, args)))
}

pub(crate) fn try_parse_no_paren_invocant_colon_call<'a>(
    name: &str,
    first_arg: Expr,
    rest_after_first_arg: &'a str,
) -> PResult<'a, Option<Expr>> {
    let (r_ws, _) = ws(rest_after_first_arg)?;
    if !r_ws.starts_with(':') || r_ws.starts_with("::") {
        return Ok((rest_after_first_arg, None));
    }
    // If the first arg is a colonpair (FatArrow Pair like :r, :!d, :name(val)),
    // a following ':' is another colonpair, not an invocant colon.
    if matches!(
        &first_arg,
        Expr::Binary {
            op: crate::token_kind::TokenKind::FatArrow,
            ..
        }
    ) {
        return Ok((rest_after_first_arg, None));
    }

    let after_colon = &r_ws[1..];

    // If the colon is immediately followed by an identifier char, `!`, or a sigil,
    // it's a colonpair (e.g. `:r`, `:!d`, `:$var`), not an invocant colon.
    if let Some(c) = after_colon.chars().next()
        && (c.is_alphabetic()
            || c == '_'
            || c == '!'
            || c == '$'
            || c == '@'
            || c == '%'
            || c == '&')
    {
        return Ok((rest_after_first_arg, None));
    }
    let (mut r, _) = ws(after_colon)?;

    if let Some(after_comma) = r.strip_prefix(',') {
        let (after_ws, _) = ws(after_comma)?;
        r = after_ws;
    }

    let mut args = Vec::new();
    if !(r.is_empty()
        || r.starts_with(';')
        || r.starts_with('}')
        || r.starts_with(')')
        || r.starts_with(']')
        || is_stmt_modifier_ahead(r))
    {
        let (r2, first_method_arg) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected method argument after invocant colon",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        args.push(first_method_arg);
        r = r2;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                break;
            }
            let r2 = &r2[1..];
            let (r2, _) = ws(r2)?;
            if r2.is_empty()
                || r2.starts_with(';')
                || r2.starts_with('}')
                || r2.starts_with(')')
                || r2.starts_with(']')
                || is_stmt_modifier_ahead(r2)
            {
                break;
            }
            let (r2, arg) = expression(r2).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected method argument after ','",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r2.len())),
                exception: None,
            })?;
            args.push(arg);
            r = r2;
        }
    }

    Ok((
        r,
        Some(Expr::MethodCall {
            target: Box::new(first_arg),
            name: Symbol::intern(name),
            args,
            modifier: None,
            quoted: false,
        }),
    ))
}

pub(crate) fn parse_listop_arg(input: &str) -> PResult<'_, Expr> {
    if is_stmt_modifier_ahead(input) {
        return Err(PError::expected("listop argument"));
    }

    call_arg_expr(input)
}

pub(crate) fn make_call_expr_from_listop_args<'a>(
    rest: &'a str,
    input: &'a str,
    name: String,
) -> PResult<'a, Expr> {
    let (r, first) = parse_listop_arg(rest).map_err(|err| PError {
        messages: merge_expected_messages("expected listop argument expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(rest.len())),
        exception: err.exception,
    })?;
    let (r, invocant_colon_call) = try_parse_no_paren_invocant_colon_call(&name, first.clone(), r)?;
    if let Some(method_call) = invocant_colon_call {
        return Ok((r, method_call));
    }
    let mut args = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        // Adjacent colonpairs without commas: foo :a :b :c or foo :a:b:c
        if r2.starts_with(':')
            && !r2.starts_with("::")
            && let Ok((r3, arg)) = parse_listop_arg(r2)
        {
            args.push(arg);
            r = r3;
            continue;
        }
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        if r2.is_empty()
            || r2.starts_with(';')
            || r2.starts_with('}')
            || r2.starts_with(')')
            || is_stmt_modifier_ahead(r2)
        {
            break;
        }
        let (r2, arg) = parse_listop_arg(r2).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected listop argument expression after ','",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r2.len())),
            exception: err.exception,
        })?;
        args.push(arg);
        r = r2;
    }
    Ok((r, make_call_expr(name, input, args)))
}
