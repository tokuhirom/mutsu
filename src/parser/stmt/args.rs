use super::super::expr::expression;
use super::super::helpers::{split_angle_words, ws};
use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_char};
use super::super::primary::misc::reduction_call_style_expr;
use super::super::primary::parse_block_body;

use crate::ast::{CallArg, Expr};
use crate::value::Value;

use super::{ident, is_stmt_modifier_keyword, try_parse_assign_expr};

/// Parse call arguments for statement-level function calls.
/// Handles positional args, named args (fat arrow and colon pairs).
pub(super) fn parse_stmt_call_args(input: &str) -> PResult<'_, Vec<CallArg>> {
    let mut args = Vec::new();
    let rest = input;

    // Check for parens-style call
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            let (r, _) = parse_char(r, ')')?;
            return Ok((r, args));
        }
        let (r, first_arg) = parse_single_call_arg(r).map_err(|err| PError {
            messages: merge_expected_messages("expected first call argument", &err.messages),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // Check for invocant colon: foo($obj:) or foo($obj: $a, $b)
        {
            let (r_ws, _) = ws(r)?;
            if r_ws.starts_with(':') && !r_ws.starts_with("::") {
                let after_colon = &r_ws[1..];
                let (after_ws, _) = ws(after_colon)?;
                // Invocant colon: extract the expression from the first arg
                let invocant_expr = match first_arg {
                    CallArg::Positional(expr) => expr,
                    _ => {
                        return Err(PError::expected(
                            "positional argument before invocant colon",
                        ));
                    }
                };
                args.push(CallArg::Invocant(invocant_expr));
                if after_ws.starts_with(')') {
                    let (r2, _) = parse_char(after_ws, ')')?;
                    return Ok((r2, args));
                }
                // Parse remaining args after invocant colon
                let (r2, more) = parse_remaining_call_args(after_ws).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected call arguments after invocant colon",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(after_ws.len())),
                    exception: None,
                })?;
                args.extend(more);
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, ')')?;
                return Ok((r2, args));
            }
        }
        args.push(first_arg);
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if r2.starts_with(')') {
                let (r2, _) = parse_char(r2, ')')?;
                // Check for additional args after closing paren
                let (r2, _) = ws(r2)?;
                if r2.starts_with(',') {
                    let (r2, _) = parse_char(r2, ',')?;
                    let (r2, _) = ws(r2)?;
                    let (r2, more) = parse_remaining_call_args(r2).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected call arguments after closing paren",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r2.len())),
                        exception: None,
                    })?;
                    args.extend(more);
                    return Ok((r2, args));
                }
                return Ok((r2, args));
            }
            // Adjacent colonpairs without commas: foo(:a :b :c) or foo(:a:b:c)
            if r2.starts_with(':')
                && !r2.starts_with("::")
                && let Ok((r3, arg)) = parse_single_call_arg(r2)
            {
                args.push(arg);
                r = r3;
                continue;
            }
            if !r2.starts_with(',') {
                // Try closing paren
                return Err(PError::expected("',' or ')'"));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(')') {
                let (r2, _) = parse_char(r2, ')')?;
                return Ok((r2, args));
            }
            let (r2, arg) = parse_single_call_arg(r2).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected call argument after ','",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r2.len())),
                exception: None,
            })?;
            args.push(arg);
            r = r2;
        }
    }

    // No-paren call args
    // Check if there are args at all
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        return Ok((rest, args));
    }

    let (rest, more) = parse_remaining_call_args(rest)?;
    args.extend(more);
    Ok((rest, args))
}

/// Parse statement call args without treating leading `(` as function call parens.
/// Used when there was whitespace between function name and args (listop form).
pub(super) fn parse_stmt_call_args_no_paren(input: &str) -> PResult<'_, Vec<CallArg>> {
    if input.starts_with(';') || input.is_empty() || input.starts_with('}') {
        return Ok((input, Vec::new()));
    }
    parse_remaining_call_args(input)
}

/// Parse remaining comma-separated call args.
pub(super) fn parse_remaining_call_args(input: &str) -> PResult<'_, Vec<CallArg>> {
    let mut args = Vec::new();
    let (mut rest, first) = parse_single_call_arg(input).map_err(|err| PError {
        messages: merge_expected_messages("expected first call argument", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
        exception: None,
    })?;
    let (rest_ws, _) = ws(rest)?;
    if rest_ws.starts_with(':') && !rest_ws.starts_with("::") {
        let invocant_expr = match first {
            CallArg::Positional(expr) => expr,
            _ => {
                return Err(PError::expected(
                    "positional argument before invocant colon",
                ));
            }
        };
        args.push(CallArg::Invocant(invocant_expr));
        let after_colon = &rest_ws[1..];
        let (after_colon, _) = ws(after_colon)?;
        if after_colon.starts_with(';')
            || after_colon.is_empty()
            || after_colon.starts_with('}')
            || after_colon.starts_with(')')
        {
            return Ok((after_colon, args));
        }
        let (after_args, mut more) = parse_remaining_call_args(after_colon)?;
        args.append(&mut more);
        return Ok((after_args, args));
    }
    args.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, args));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        // Check for end
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, args));
        }
        if is_stmt_modifier_keyword(r) {
            return Ok((r, args));
        }
        let (r, arg) = parse_single_call_arg(r).map_err(|err| PError {
            messages: merge_expected_messages("expected call argument after ','", &err.messages),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        args.push(arg);
        rest = r;
    }
}

/// Parse a single call argument (named or positional).
pub(super) fn parse_single_call_arg(input: &str) -> PResult<'_, CallArg> {
    // Capture slip: |var — flatten a capture into the argument list
    if input.starts_with('|') && !input.starts_with("||") {
        let after_pipe = &input[1..];
        // Must be followed by an identifier (sigilless capture variable)
        if let Ok((r, name)) = ident(after_pipe) {
            return Ok((r, CallArg::Slip(Expr::BareWord(name))));
        }
    }

    // Colon pair: :name(expr) or :name or :!name or :name[...]
    if input.starts_with(':') && !input.starts_with("::") {
        let r = &input[1..];
        // :!name (negated boolean)
        if let Some(stripped) = r.strip_prefix('!') {
            let (r, name) = ident(stripped)?;
            return Ok((
                r,
                CallArg::Named {
                    name,
                    value: Some(Expr::Literal(Value::Bool(false))),
                },
            ));
        }
        // :$var / :@var / :%var / :&var (autopair from variable, with twigil support)
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&') {
            let sigil = &r[..1];
            let after_sigil = &r[1..];
            // Check for twigils: *, ?, ^, =, ~, :
            let (after_twigil, twigil) = if after_sigil.starts_with('*')
                || after_sigil.starts_with('?')
                || after_sigil.starts_with('^')
                || after_sigil.starts_with('=')
                || after_sigil.starts_with('~')
            {
                (&after_sigil[1..], &after_sigil[..1])
            } else if after_sigil.starts_with(':') && !after_sigil.starts_with("::") {
                (&after_sigil[1..], ":")
            } else {
                (after_sigil, "")
            };
            if let Ok((rest, var_name)) = ident(after_twigil) {
                let full_var_name = if twigil.is_empty() {
                    var_name.clone()
                } else {
                    format!("{}{}", twigil, var_name)
                };
                let var_expr = match sigil {
                    "$" => Expr::Var(full_var_name),
                    "@" => Expr::ArrayVar(full_var_name),
                    "%" => Expr::HashVar(full_var_name),
                    "&" => Expr::CodeVar(full_var_name),
                    _ => unreachable!(),
                };
                return Ok((
                    rest,
                    CallArg::Named {
                        name: var_name,
                        value: Some(var_expr),
                    },
                ));
            }
        }
        // :name followed by ( or [ or nothing
        if let Ok((r, name)) = ident(r) {
            // Check for statement modifier keywords - don't parse as named arg
            if matches!(
                name.as_str(),
                "if" | "unless" | "for" | "while" | "until" | "given" | "when"
            ) {
                // Not a named arg, fall through to positional
            } else {
                // :name(expr) or :name(expr,) or :name(expr1, expr2, ...)
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, val) = expression(r).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected named argument value",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r.len())),
                        exception: None,
                    })?;
                    let (r, _) = ws(r)?;
                    // Check for comma — makes a list: :name(a, b) or :name(a,)
                    if r.starts_with(',') {
                        let mut items = vec![val];
                        let mut r = r;
                        while r.starts_with(',') {
                            let (r2, _) = parse_char(r, ',')?;
                            let (r2, _) = ws(r2)?;
                            if r2.starts_with(')') {
                                r = r2;
                                break;
                            }
                            let (r2, next) = expression(r2).map_err(|err| PError {
                                messages: merge_expected_messages(
                                    "expected list item in named argument",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r2.len())),
                                exception: None,
                            })?;
                            let (r2, _) = ws(r2)?;
                            items.push(next);
                            r = r2;
                        }
                        let (r, _) = parse_char(r, ')')?;
                        return Ok((
                            r,
                            CallArg::Named {
                                name,
                                value: Some(Expr::ArrayLiteral(items)),
                            },
                        ));
                    }
                    let (r, _) = parse_char(r, ')')?;
                    return Ok((
                        r,
                        CallArg::Named {
                            name,
                            value: Some(val),
                        },
                    ));
                }
                // :name[items]
                if r.starts_with('[') {
                    let (r, _) = parse_char(r, '[')?;
                    let (r, _) = ws(r)?;
                    let mut items = Vec::new();
                    if !r.starts_with(']') {
                        let (r2, first) = expression(r).map_err(|err| PError {
                            messages: merge_expected_messages(
                                "expected first list item in named argument",
                                &err.messages,
                            ),
                            remaining_len: err.remaining_len.or(Some(r.len())),
                            exception: None,
                        })?;
                        items.push(first);
                        let mut r = r2;
                        loop {
                            let (r2, _) = ws(r)?;
                            if r2.starts_with(']') {
                                let (r2, _) = parse_char(r2, ']')?;
                                return Ok((
                                    r2,
                                    CallArg::Named {
                                        name,
                                        value: Some(Expr::ArrayLiteral(items)),
                                    },
                                ));
                            }
                            let (r2, _) = parse_char(r2, ',')?;
                            let (r2, _) = ws(r2)?;
                            let (r2, next) = expression(r2).map_err(|err| PError {
                                messages: merge_expected_messages(
                                    "expected list item after ',' in named argument",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r2.len())),
                                exception: None,
                            })?;
                            items.push(next);
                            r = r2;
                        }
                    }
                    let (r, _) = parse_char(r, ']')?;
                    return Ok((
                        r,
                        CallArg::Named {
                            name,
                            value: Some(Expr::ArrayLiteral(items)),
                        },
                    ));
                }
                // :name{ ... } (block-valued named argument)
                if r.starts_with('{') {
                    let (r, body) = parse_block_body(r)?;
                    return Ok((
                        r,
                        CallArg::Named {
                            name,
                            value: Some(Expr::AnonSub { body, is_rw: false }),
                        },
                    ));
                }
                // :name<word> or :name<words> (angle bracket form)
                if r.starts_with('<') && !r.starts_with("<<") {
                    let (r, _) = parse_char(r, '<')?;
                    let end = r
                        .find('>')
                        .ok_or_else(|| PError::expected("'>' closing angle bracket"))?;
                    let content = &r[..end];
                    let r = &r[end + 1..];
                    let words = split_angle_words(content);
                    if words.len() == 1 {
                        return Ok((
                            r,
                            CallArg::Named {
                                name,
                                value: Some(Expr::Literal(Value::Str(words[0].to_string()))),
                            },
                        ));
                    }
                    let items = words
                        .iter()
                        .map(|w| Expr::Literal(Value::Str(w.to_string())))
                        .collect();
                    return Ok((
                        r,
                        CallArg::Named {
                            name,
                            value: Some(Expr::ArrayLiteral(items)),
                        },
                    ));
                }
                // :name (boolean true)
                return Ok((r, CallArg::Named { name, value: None }));
            }
        }
    }

    // Fat arrow: name => expr (becomes named arg)
    // Check: identifier followed by =>
    if let Ok((r, name)) = ident(input) {
        let (r2, _) = ws(r)?;
        if let Some(stripped) = r2.strip_prefix("=>") {
            let (r2, _) = ws(stripped)?;
            let (r2, val) = expression(r2).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected fat-arrow argument value",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r2.len())),
                exception: None,
            })?;
            return Ok((
                r2,
                CallArg::Named {
                    name,
                    value: Some(val),
                },
            ));
        }
    }

    // Positional argument — try assignment expression first ($x = expr).
    // But do not consume a prefix before a fat-arrow chain (e.g. `2 => "x" => {...}`).
    if let Ok((rest, assign_expr)) = try_parse_assign_expr(input) {
        let (rest_ws, _) = ws(rest)?;
        if !rest_ws.starts_with("=>") || rest_ws.starts_with("==>") {
            return Ok((rest, CallArg::Positional(assign_expr)));
        }
    }
    // Prefer regular expression parsing for unary-minus angle terms like `-<42+2i>`.
    // The reduction call-style parser can misinterpret this shape in statement arg context.
    let parsed_expr = if input.starts_with("-<") || input.starts_with("−<") {
        expression(input)
    } else {
        reduction_call_style_expr(input).or_else(|_| expression(input))
    };
    let (rest, expr) = parsed_expr.map_err(|err| PError {
        messages: merge_expected_messages("expected positional argument expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
        exception: None,
    })?;
    Ok((rest, CallArg::Positional(expr)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_call_args_accepts_bracket_metaop_assign_expr() {
        let (rest, args) = parse_stmt_call_args("$y [R/]= 1, 1/5, \"desc\";").unwrap();
        assert_eq!(rest, ";");
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn parse_call_args_no_paren_accepts_bracket_metaop_assign_expr() {
        let (rest, args) = parse_stmt_call_args_no_paren("$y [R/]= 1, 1/5, \"desc\";").unwrap();
        assert_eq!(rest, ";");
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn parse_call_args_no_paren_accepts_bracket_metaop_assign_expr_with_bracket_desc() {
        let input = "$y [R/]= 1, 1/5, \"[R/]= works correctly (1)\";";
        let (rest, args) = parse_stmt_call_args_no_paren(input).unwrap();
        assert_eq!(rest, ";");
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn parse_single_call_arg_ascii_minus_angle_complex_literal() {
        let (rest, _) = parse_single_call_arg("-<42+2i>, 'x'").unwrap();
        assert_eq!(rest, ", 'x'");
    }
}
