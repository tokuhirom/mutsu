use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char};

use crate::ast::{Expr, Stmt};
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
        // For now, just treat it as a labeled block by parsing `do { ... }`
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
    if let Some(stripped) = input.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
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
            Ok((r, (None, params)))
        } else {
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
        // init
        let (rest, init) = if rest.starts_with(';') {
            (rest, None)
        } else {
            let (r, s) = statement(rest)?;
            (r, Some(Box::new(s)))
        };
        // The init statement already consumed its semicolon if it's a decl
        // But for safety, consume any extra semicolons
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
        // step
        let (rest, step) = if rest.starts_with(')') {
            (rest, None)
        } else {
            let (r, e) = expression(rest)?;
            (r, Some(e))
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
        let (r, body) = block(r)?;
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
        let (r, body) = block(r)?;
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
    Err(PError::expected("while or until after repeat"))
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

/// Parse `with`/`without` statement.
pub(super) fn with_stmt(input: &str) -> PResult<'_, Stmt> {
    let is_without = keyword("without", input).is_some();
    let rest = keyword("with", input)
        .or_else(|| keyword("without", input))
        .ok_or_else(|| PError::expected("with/without statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;

    let cond = Expr::MethodCall {
        target: Box::new(cond),
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
        let (r, orwith_cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, orwith_body) = block(r)?;
        let orwith_cond = Expr::MethodCall {
            target: Box::new(orwith_cond),
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
                then_branch: orwith_body,
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
            then_branch: body,
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
