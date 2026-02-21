use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};

use crate::ast::{AssignOp, Expr, Stmt};
use crate::token_kind::TokenKind;

use super::{block, ident, keyword, parse_comma_or_expr, statement, var_name};

pub(super) fn if_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("if", input).ok_or_else(|| PError::expected("if statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, then_branch) = block(rest)?;
    let (rest, _) = ws(rest)?;

    // elsif chain
    let (rest, else_branch) = parse_elsif_chain(rest)?;

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        },
    ))
}

pub(super) fn parse_elsif_chain(input: &str) -> PResult<'_, Vec<Stmt>> {
    if let Some(r) = keyword("elsif", input) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, then_branch) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, else_branch) = parse_elsif_chain(r)?;
        return Ok((
            r,
            vec![Stmt::If {
                cond,
                then_branch,
                else_branch,
            }],
        ));
    }
    if let Some(r) = keyword("else", input) {
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((r, body));
    }
    Ok((input, Vec::new()))
}

/// Parse `unless` statement.
pub(super) fn unless_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unless", input).ok_or_else(|| PError::expected("unless statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // unless cannot have else/elsif/orwith — check but consume the trailing clause
    let (check, _) = ws(rest)?;
    for kw in &["else", "elsif", "orwith"] {
        if let Some(r) = keyword(kw, check) {
            // Consume the rest of the invalid clause to produce a hard error
            let (r, _) = ws(r)?;
            // Skip condition (if any) and block
            let r = if kw != &"else" {
                if let Ok((r, _)) = expression(r) { r } else { r }
            } else {
                r
            };
            let (r, _) = ws(r)?;
            let r = if let Ok((r, _)) = block(r) { r } else { r };
            return Ok((
                r,
                Stmt::Die(Expr::Literal(crate::value::Value::Str(format!(
                    "X::Syntax::UnlessElse: unless does not allow '{kw}'"
                )))),
            ));
        }
    }
    Ok((
        rest,
        Stmt::If {
            cond: Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond),
            },
            then_branch: body,
            else_branch: Vec::new(),
        },
    ))
}

/// Parse `for` loop.
/// Parse a labeled loop: LABEL: for/while/until/loop/repeat ...
pub(super) fn labeled_loop_stmt(input: &str) -> PResult<'_, Stmt> {
    // Label must be all uppercase or mixed case identifier followed by ':'
    let (rest, label) = ident(input)?;
    // Labels are typically ALL CAPS like FOO, DONE, OUT, IN
    // but we need to check it's followed by : and then a loop keyword
    let (rest, _) = ws(rest)?;
    if !rest.starts_with(':') || rest.starts_with("::") {
        return Err(PError::expected("labeled loop"));
    }
    let rest = &rest[1..]; // consume ':'
    let (rest, _) = ws(rest)?;

    // Check which loop keyword follows
    if let Some(r) = keyword("for", rest) {
        let (r, _) = ws1(r)?;
        let (r, iterable) = parse_comma_or_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::For {
                iterable,
                param,
                params,
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond,
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("loop", rest) {
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: None,
                step: None,
                body,
                repeat: false,
                label: Some(label),
            },
        ));
    }

    // Label before `do` block: `A: do { ... }`
    if keyword("do", rest).is_some() {
        // Parse the rest as a statement and wrap with label
        // TODO: Represent labeled `do { ... }` with a dedicated AST node instead of
        // lowering it to a dummy `Stmt::For` carrying `Nil`.
        let r = keyword("do", rest).unwrap();
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::For {
                iterable: Expr::ArrayLiteral(vec![Expr::Literal(crate::value::Value::Nil)]),
                param: None,
                params: Vec::new(),
                body,
                label: Some(label),
            },
        ));
    }
    // Label before bare block: `A: { ... }`
    // TODO: Represent labeled bare blocks directly instead of lowering to
    // a dummy `Stmt::For` carrying `Nil`.
    if rest.starts_with('{') {
        let (r, body) = block(rest)?;
        return Ok((
            r,
            Stmt::For {
                iterable: Expr::ArrayLiteral(vec![Expr::Literal(crate::value::Value::Nil)]),
                param: None,
                params: Vec::new(),
                body,
                label: Some(label),
            },
        ));
    }

    Err(PError::expected("labeled loop"))
}

/// Parse for loop parameters: -> $param or -> $a, $b
pub(super) fn parse_for_params(input: &str) -> PResult<'_, (Option<String>, Vec<String>)> {
    fn skip_pointy_return_type<'a>(mut r: &'a str) -> PResult<'a, ()> {
        let (r2, _) = ws(r)?;
        r = r2;
        if let Some(after_arrow) = r.strip_prefix("-->") {
            let (after_arrow, _) = ws(after_arrow)?;
            // TODO: Parse full Raku type expressions for pointy return types.
            // Current implementation accepts only simple identifier-like names.
            let (after_arrow, _type_name) = take_while1(after_arrow, |c: char| {
                c.is_alphanumeric() || c == '_' || c == ':'
            })?;
            let (after_arrow, _) = ws(after_arrow)?;
            Ok((after_arrow, ()))
        } else {
            Ok((r, ()))
        }
    }

    if let Some(stripped) = input.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        // Zero-parameter pointy block: for @a -> { ... }
        if r.starts_with('{') {
            return Ok((r, (None, Vec::new())));
        }
        let (r, first) = parse_pointy_param(r)?;
        let (r, _) = ws(r)?;
        if r.starts_with(',') {
            let mut params = vec![first];
            let mut r = r;
            loop {
                let (r2, _) = parse_char(r, ',')?;
                let (r2, _) = ws(r2)?;
                let (r2, next) = parse_pointy_param(r2)?;
                params.push(next);
                let (r2, _) = ws(r2)?;
                if !r2.starts_with(',') {
                    r = r2;
                    break;
                }
                r = r2;
            }
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((r, (None, params)))
        } else {
            let (r, _) = skip_pointy_return_type(r)?;
            Ok((r, (Some(first), Vec::new())))
        }
    } else {
        Ok((input, (None, Vec::new())))
    }
}

pub(super) fn for_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("for", input).ok_or_else(|| PError::expected("for statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, iterable) = parse_comma_or_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, (param, params)) = parse_for_params(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::For {
            iterable,
            param,
            params,
            body,
            label: None,
        },
    ))
}

pub(super) fn parse_pointy_param(input: &str) -> PResult<'_, String> {
    // Sigilless parameter: \name
    if let Some(r) = input.strip_prefix('\\') {
        let (rest, name) = ident(r)?;
        return Ok((rest, name));
    }
    // Optional type constraint before the variable
    let rest = input;
    let rest = if let Ok((r, _tc)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            r2
        } else {
            rest
        }
    } else {
        rest
    };
    let (rest, name) = var_name(rest)?;
    // Optional marker on pointy params: $x? / $x!
    let mut rest = if rest.starts_with('?') || rest.starts_with('!') {
        &rest[1..]
    } else {
        rest
    };

    // Optional parameter traits: `is rw`, `is copy`, ...
    // Keep parsing permissive here and ignore trait semantics for now.
    loop {
        let (r, _) = ws(rest)?;
        let Some(after_is) = keyword("is", r) else {
            rest = r;
            break;
        };
        let (after_is, _) = ws1(after_is)?;
        let (after_is, _trait_name) = ident(after_is)?;
        rest = after_is;
    }

    // Optional default value: `$x = expr`
    let (r, _) = ws(rest)?;
    if let Some(after_eq) = r.strip_prefix('=')
        && !after_eq.starts_with('>')
    {
        let (after_eq, _) = ws(after_eq)?;
        let (after_default, _default_expr) = expression(after_eq)?;
        rest = after_default;
    } else {
        rest = r;
    }

    Ok((rest, name))
}

/// Parse `while` loop.
pub(super) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("while", input).ok_or_else(|| PError::expected("while statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::While {
            cond,
            body,
            label: None,
        },
    ))
}

/// Parse `until` loop.
pub(super) fn until_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("until", input).ok_or_else(|| PError::expected("until statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::While {
            cond: Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond),
            },
            body,
            label: None,
        },
    ))
}

/// Parse C-style `loop` or infinite loop.
pub(super) fn loop_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("loop", input).ok_or_else(|| PError::expected("loop statement"))?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        // init: may be comma-separated list of statements
        let (rest, init) = if let Some(rest_after_semi) = rest.strip_prefix(';') {
            (rest_after_semi, None)
        } else {
            let (mut r, first) = statement(rest)?;
            let mut stmts = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, s) = statement(r2)?;
                    stmts.push(s);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if stmts.len() == 1 {
                (r, Some(Box::new(stmts.into_iter().next().unwrap())))
            } else {
                (r, Some(Box::new(Stmt::Block(stmts))))
            }
        };
        let (rest, _) = ws(rest)?;
        // cond
        let (rest, cond) = if let Some(rest) = rest.strip_prefix(';') {
            (rest, None)
        } else {
            let (r, e) = expression(rest)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ';')?;
            (r, Some(e))
        };
        let (rest, _) = ws(rest)?;
        // step: may be comma-separated list of expressions
        let (rest, step) = if rest.starts_with(')') {
            (rest, None)
        } else {
            let (mut r, first) = expression(rest)?;
            let mut exprs = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, e) = expression(r2)?;
                    exprs.push(e);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if exprs.len() == 1 {
                (r, Some(exprs.into_iter().next().unwrap()))
            } else {
                // Wrap multiple step expressions in a DoBlock
                let stmts: Vec<Stmt> = exprs.into_iter().map(Stmt::Expr).collect();
                (
                    r,
                    Some(Expr::DoBlock {
                        body: stmts,
                        label: None,
                    }),
                )
            }
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let (rest, _) = ws(rest)?;
        let (rest, body) = block(rest)?;
        return Ok((
            rest,
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat: false,
                label: None,
            },
        ));
    }
    // Infinite loop
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Loop {
            init: None,
            cond: None,
            step: None,
            body,
            repeat: false,
            label: None,
        },
    ))
}

/// Parse `repeat while/until` loop.
pub(super) fn repeat_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("repeat", input).ok_or_else(|| PError::expected("repeat statement"))?;
    let (rest, _) = ws(rest)?;

    // repeat while/until COND { BODY }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(cond),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, params)) = parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }

    // repeat { BODY } while/until COND
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(cond),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    Err(PError::fatal(
        "X::Syntax::Missing: \"while\" or \"until\" required after repeat".to_string(),
    ))
}

/// Parse `given`/`when`/`default`.
pub(super) fn given_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("given", input).ok_or_else(|| PError::expected("given statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, topic) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Given { topic, body }))
}

pub(super) fn when_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("when", input).ok_or_else(|| PError::expected("when statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

pub(super) fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}

/// Build a `$_ = <expr>` assignment statement for topicalization.
fn topicalize(expr: &Expr) -> Stmt {
    Stmt::Assign {
        name: "_".to_string(),
        op: AssignOp::Assign,
        expr: expr.clone(),
    }
}

/// Parse `with`/`without` statement.
pub(super) fn with_stmt(input: &str) -> PResult<'_, Stmt> {
    let is_without = keyword("without", input).is_some();
    let rest = keyword("with", input)
        .or_else(|| keyword("without", input))
        .ok_or_else(|| PError::expected("with/without statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond_expr) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;

    // Prepend $_ = <cond_expr> to the body for topicalization
    let mut with_body = vec![topicalize(&cond_expr)];
    with_body.extend(body);

    let cond = Expr::MethodCall {
        target: Box::new(cond_expr),
        name: "defined".to_string(),
        args: Vec::new(),
        modifier: None,
    };
    let cond = if is_without {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
        }
    } else {
        cond
    };

    // Parse orwith / else chains
    let (rest, _) = ws(rest)?;
    let (rest, else_branch) = if keyword("orwith", rest).is_some() {
        let r = keyword("orwith", rest).unwrap();
        let (r, _) = ws1(r)?;
        let (r, orwith_cond_expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, orwith_body) = block(r)?;

        // Prepend $_ = <orwith_cond_expr> to the body
        let mut orwith_with_body = vec![topicalize(&orwith_cond_expr)];
        orwith_with_body.extend(orwith_body);

        let orwith_cond = Expr::MethodCall {
            target: Box::new(orwith_cond_expr),
            name: "defined".to_string(),
            args: Vec::new(),
            modifier: None,
        };
        let (r, _) = ws(r)?;
        let (r, orwith_else) = if keyword("else", r).is_some() {
            let r2 = keyword("else", r).unwrap();
            let (r2, _) = ws(r2)?;
            let (r2, else_body) = block(r2)?;
            (r2, else_body)
        } else {
            (r, Vec::new())
        };
        (
            r,
            vec![Stmt::If {
                cond: orwith_cond,
                then_branch: orwith_with_body,
                else_branch: orwith_else,
            }],
        )
    } else if keyword("else", rest).is_some() {
        let r = keyword("else", rest).unwrap();
        let (r, _) = ws(r)?;
        let (r, else_body) = block(r)?;
        (r, else_body)
    } else {
        (rest, Vec::new())
    };

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch: with_body,
            else_branch,
        },
    ))
}

/// Parse `react` block.
pub(super) fn react_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("react", input).ok_or_else(|| PError::expected("react block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::React { body }))
}

/// Parse `whenever` block.
pub(super) fn whenever_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("whenever", input).ok_or_else(|| PError::expected("whenever block"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, supply) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param) = if let Some(stripped) = rest.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        let (r, name) = var_name(r)?;
        (r, Some(name))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Whenever {
            supply,
            param,
            body,
        },
    ))
}
