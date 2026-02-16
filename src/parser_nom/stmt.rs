use super::parse_result::{PError, PResult, opt_char, parse_char, take_while_opt, take_while1};

use crate::ast::{AssignOp, CallArg, Expr, ParamDef, PhaserKind, Stmt};
use crate::lexer::TokenKind;
use crate::value::Value;

use super::expr::expression;
use super::helpers::{ws, ws1};

/// Check if a byte is a valid identifier continuation character.
fn is_ident_char(b: Option<u8>) -> bool {
    match b {
        Some(c) => c.is_ascii_alphanumeric() || c == b'_' || c == b'-',
        None => false,
    }
}

/// Try to match a keyword at the start of input, ensuring word boundary.
fn keyword<'a>(kw: &str, input: &'a str) -> Option<&'a str> {
    if input.starts_with(kw) && !is_ident_char(input.as_bytes().get(kw.len()).copied()) {
        Some(&input[kw.len()..])
    } else {
        None
    }
}

/// Parse an identifier (alphanumeric, _, -).
fn ident(input: &str) -> PResult<'_, String> {
    let (rest, name) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    Ok((rest, name.to_string()))
}

/// Parse a qualified identifier (Foo::Bar::Baz).
fn qualified_ident(input: &str) -> PResult<'_, String> {
    let (mut rest, name) = ident(input)?;
    let mut full = name;
    while rest.starts_with("::") {
        let r = &rest[2..];
        if let Ok((r2, part)) = ident(r) {
            full.push_str("::");
            full.push_str(&part);
            rest = r2;
        } else {
            break;
        }
    }
    Ok((rest, full))
}

/// Parse a variable name ($x, @arr, %hash) and return just the name part.
fn var_name(input: &str) -> PResult<'_, String> {
    if input.starts_with('$') || input.starts_with('@') || input.starts_with('%') {
        let r = &input[1..];
        // Handle twigils
        let (r, twigil) =
            if r.starts_with('*') || r.starts_with('?') || r.starts_with('!') || r.starts_with('^')
            {
                (&r[1..], &r[..1])
            } else {
                (r, "")
            };
        // Handle $_ special
        if input.starts_with('$')
            && r.starts_with('_')
            && (r.len() == 1
                || !r
                    .as_bytes()
                    .get(1)
                    .is_some_and(|c| c.is_ascii_alphanumeric()))
        {
            return Ok((&r[1..], "_".to_string()));
        }
        let (rest, name) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let full = if twigil.is_empty() {
            name.to_string()
        } else {
            format!("{}{}", twigil, name)
        };
        Ok((rest, full))
    } else {
        Err(PError::expected("variable name"))
    }
}

/// Parse a block: { stmts }
fn block(input: &str) -> PResult<'_, Vec<Stmt>> {
    let (input, _) = parse_char(input, '{')?;
    let (input, stmts) = stmt_list(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = parse_char(input, '}')?;
    Ok((input, stmts))
}

/// Public accessor for stmt_list (used by primary.rs for block expressions).
pub(super) fn stmt_list_pub(input: &str) -> PResult<'_, Vec<Stmt>> {
    stmt_list(input)
}

/// Public accessor for ident (used by primary.rs for hash literal detection).
pub(super) fn ident_pub(input: &str) -> PResult<'_, String> {
    ident(input)
}

/// Public accessor for var_name (used by primary.rs for anon sub params).
pub(super) fn var_name_pub(input: &str) -> PResult<'_, String> {
    var_name(input)
}

/// Public accessor for statement (used by primary.rs for do-statement expressions).
pub(super) fn statement_pub(input: &str) -> PResult<'_, Stmt> {
    statement(input)
}

/// Parse a list of statements (inside a block or at program level).
fn stmt_list(input: &str) -> PResult<'_, Vec<Stmt>> {
    let mut stmts = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        // Consume any standalone semicolons
        let r = consume_semicolons(r);
        let (r, _) = ws(r)?;
        // End of block or input
        if r.is_empty() || r.starts_with('}') {
            return Ok((r, stmts));
        }
        match statement(r) {
            Ok((r, stmt)) => {
                stmts.push(stmt);
                rest = r;
            }
            Err(e) => {
                let consumed = input.len() - r.len();
                let line_num = input[..consumed].matches('\n').count() + 1;
                let context: String = r.chars().take(80).collect();
                return Err(PError::expected(&format!(
                    "statement at line {} (after {} stmts): {} — near: {:?}",
                    line_num,
                    stmts.len(),
                    e.message,
                    context
                )));
            }
        }
    }
}

/// Consume zero or more semicolons.
fn consume_semicolons(mut input: &str) -> &str {
    while input.starts_with(';') {
        input = &input[1..];
        // Also consume whitespace after semicolons
        if let Ok((r, _)) = ws(input) {
            input = r;
        }
    }
    input
}

/// Parse call arguments for statement-level function calls.
/// Handles positional args, named args (fat arrow and colon pairs).
fn parse_stmt_call_args(input: &str) -> PResult<'_, Vec<CallArg>> {
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
        let (r, first_arg) = parse_single_call_arg(r)?;
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
                    let (r2, more) = parse_remaining_call_args(r2)?;
                    args.extend(more);
                    return Ok((r2, args));
                }
                return Ok((r2, args));
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
            let (r2, arg) = parse_single_call_arg(r2)?;
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
fn parse_stmt_call_args_no_paren(input: &str) -> PResult<'_, Vec<CallArg>> {
    if input.starts_with(';') || input.is_empty() || input.starts_with('}') {
        return Ok((input, Vec::new()));
    }
    parse_remaining_call_args(input)
}

/// Parse remaining comma-separated call args.
fn parse_remaining_call_args(input: &str) -> PResult<'_, Vec<CallArg>> {
    let mut args = Vec::new();
    let (mut rest, first) = parse_single_call_arg(input)?;
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
        let (r, arg) = parse_single_call_arg(r)?;
        args.push(arg);
        rest = r;
    }
}

/// Parse a single call argument (named or positional).
fn parse_single_call_arg(input: &str) -> PResult<'_, CallArg> {
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
        // :name followed by ( or [ or nothing
        if let Ok((r, name)) = ident(r) {
            // Check for statement modifier keywords - don't parse as named arg
            if matches!(
                name.as_str(),
                "if" | "unless" | "for" | "while" | "until" | "given" | "when"
            ) {
                // Not a named arg, fall through to positional
            } else {
                // :name(expr)
                if r.starts_with('(') {
                    let (r, _) = parse_char(r, '(')?;
                    let (r, _) = ws(r)?;
                    let (r, val) = expression(r)?;
                    let (r, _) = ws(r)?;
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
                        let (r2, first) = expression(r)?;
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
                            let (r2, next) = expression(r2)?;
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
            let (r2, val) = expression(r2)?;
            return Ok((
                r2,
                CallArg::Named {
                    name,
                    value: Some(val),
                },
            ));
        }
    }

    // Positional argument
    let (rest, expr) = expression(input)?;
    Ok((rest, CallArg::Positional(expr)))
}

/// Parse a `use` statement.
fn use_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("use", input).ok_or_else(|| PError::expected("use statement"))?;
    let (rest, _) = ws1(rest)?;

    // Handle `use v6`, `use v6.d`, etc.
    if rest.starts_with('v') {
        let (r, _) = take_while1(rest, |c: char| {
            c.is_alphanumeric() || c == '.' || c == '*' || c == '+'
        })?;
        let (r, _) = ws(r)?;
        let _ = opt_char(r, ';');
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Use {
                module: "v6".to_string(),
                arg: None,
            },
        ));
    }

    let (rest, module) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;

    // Optional argument
    let (rest, arg) = if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        (rest, None)
    } else if rest.starts_with('<') {
        // Angle-bracket import list
        let (r, expr) = super::primary::primary(rest)?;
        (r, Some(expr))
    } else {
        let (r, expr) = expression(rest)?;
        (r, Some(expr))
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Use { module, arg }))
}

/// Parse a `say` statement.
fn say_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("say", input).ok_or_else(|| PError::expected("say statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `print` statement.
fn print_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("print", input).ok_or_else(|| PError::expected("print statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Print(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `note` statement.
fn note_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("note", input).ok_or_else(|| PError::expected("note statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, args) = parse_expr_list(rest)?;
    let stmt = Stmt::Note(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a comma-separated expression list.
fn parse_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    let (input, first) = expression(input)?;
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, items));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        // Check for end of list
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, items));
        }
        // Check for statement modifier keywords
        if is_stmt_modifier_keyword(r) {
            return Ok((r, items));
        }
        let (r, next) = expression(r)?;
        items.push(next);
        rest = r;
    }
}

fn is_stmt_modifier_keyword(input: &str) -> bool {
    for kw in &["if", "unless", "for", "while", "until", "given", "when"] {
        if keyword(kw, input).is_some() {
            return true;
        }
    }
    false
}

/// Parse `my` variable declaration.
fn my_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("my", input)
        .or_else(|| keyword("our", input))
        .ok_or_else(|| PError::expected("my/our declaration"))?;
    let (rest, _) = ws1(rest)?;

    // my enum Foo <...>
    if let Some(r) = keyword("enum", rest) {
        let (r, _) = ws1(r)?;
        return parse_enum_decl_body(r);
    }

    // my sub name(...) { ... }
    if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws1(r)?;
        return sub_decl_body(r, false);
    }

    // my method name(...) { ... }
    if let Some(r) = keyword("method", rest) {
        let (r, _) = ws1(r)?;
        return method_decl_body(r, false);
    }

    // Optional type constraint: my Int $x
    let (rest, type_constraint) = {
        // Try to parse a type name followed by a sigil
        let saved = rest;
        if let Ok((r, tc)) = ident(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with('$')
                || r2.starts_with('@')
                || r2.starts_with('%')
                || r2.starts_with('&')
            {
                (r2, Some(tc))
            } else {
                (saved, None)
            }
        } else {
            (saved, None)
        }
    };

    // Destructuring: my ($a, $b) = expr
    if rest.starts_with('(') {
        return parse_destructuring_decl(rest);
    }

    // Parse variable
    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let is_array = sigil == b'@';
    let is_hash = sigil == b'%';
    let is_code = sigil == b'&';

    let prefix = if is_array {
        "@"
    } else if is_hash {
        "%"
    } else if is_code {
        "&"
    } else {
        ""
    };

    let (rest, name) = if sigil == b'$' || is_array || is_hash || is_code {
        let (r, n) = var_name(rest)?;
        (r, format!("{}{}", prefix, n))
    } else {
        // Sigilless variable
        let (r, n) = ident(rest)?;
        (r, n)
    };

    let (rest, _) = ws(rest)?;

    // Assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        #[cfg(test)]
        eprintln!("my_decl: taking assignment branch");
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
        };
        return parse_statement_modifier(rest, stmt);
    }
    // Binding :=
    if let Some(stripped) = rest.strip_prefix(":=") {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        let stmt = Stmt::VarDecl {
            name,
            expr,
            type_constraint,
        };
        return parse_statement_modifier(rest, stmt);
    }

    let (rest, _) = opt_char(rest, ';');
    let expr = if is_array {
        Expr::Literal(Value::Array(Vec::new()))
    } else if is_hash {
        Expr::Hash(Vec::new())
    } else {
        Expr::Literal(Value::Nil)
    };
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr,
            type_constraint,
        },
    ))
}

/// Try to parse a chained assignment expression ($var op= expr), falling back to parse_comma_or_expr.
/// This handles cases like `$a += $b += 1` and `$x = $y = $z`.
fn parse_assign_expr_or_comma(input: &str) -> PResult<'_, Expr> {
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
fn try_parse_assign_expr(input: &str) -> PResult<'_, Expr> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    if sigil != b'$' && sigil != b'@' && sigil != b'%' {
        return Err(PError::expected("assignment expression"));
    }
    let (r, var) = var_name(input)?;
    let (r2, _) = ws(r)?;
    let compound_ops: &[(&str, TokenKind)] = &[
        ("//=", TokenKind::SlashSlash),
        ("||=", TokenKind::OrOr),
        ("&&=", TokenKind::AndAnd),
        ("+=", TokenKind::Plus),
        ("-=", TokenKind::Minus),
        ("~=", TokenKind::Tilde),
        ("*=", TokenKind::Star),
    ];
    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        _ => "",
    };
    // Check compound assignment ops
    for &(op_str, ref op_kind) in compound_ops {
        if let Some(stripped) = r2.strip_prefix(op_str) {
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
                        op: op_kind.clone(),
                        right: Box::new(rhs),
                    }),
                },
            ));
        }
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
fn parse_comma_or_expr(input: &str) -> PResult<'_, Expr> {
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
    Ok((rest, first))
}

/// Parse destructuring: ($a, $b, $c) = expr
fn parse_destructuring_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    let mut names = Vec::new();
    let mut r = rest;
    loop {
        if r.starts_with(')') {
            break;
        }
        let sigil = r.as_bytes().first().copied().unwrap_or(0);
        if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
            let prefix = match sigil {
                b'@' => "@",
                b'%' => "%",
                b'&' => "&",
                _ => "",
            };
            let (r2, n) = var_name(r)?;
            names.push(format!("{}{}", prefix, n));
            let (r2, _) = ws(r2)?;
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        } else {
            return Err(PError::expected("variable sigil ($, @, %, &)"));
        }
    }
    let (rest, _) = parse_char(r, ')')?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with('=') || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, rhs) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        // Desugar into block
        let tmp_name = "@__destructure_tmp__".to_string();
        let array_bare = "__destructure_tmp__".to_string();
        let mut stmts = vec![Stmt::VarDecl {
            name: tmp_name,
            expr: rhs,
            type_constraint: None,
        }];
        for (i, var_name) in names.iter().enumerate() {
            stmts.push(Stmt::VarDecl {
                name: var_name.clone(),
                expr: Expr::Index {
                    target: Box::new(Expr::ArrayVar(array_bare.clone())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                },
                type_constraint: None,
            });
        }
        return Ok((rest, Stmt::Block(stmts)));
    }
    // No assignment
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let mut stmts = Vec::new();
    for name in &names {
        stmts.push(Stmt::VarDecl {
            name: name.clone(),
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
        });
    }
    Ok((rest, Stmt::Block(stmts)))
}

/// Parse an `if`/`elsif`/`else` chain.
fn if_stmt(input: &str) -> PResult<'_, Stmt> {
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

fn parse_elsif_chain(input: &str) -> PResult<'_, Vec<Stmt>> {
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
fn unless_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unless", input).ok_or_else(|| PError::expected("unless statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
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
fn labeled_loop_stmt(input: &str) -> PResult<'_, Stmt> {
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

    Err(PError::expected("labeled loop"))
}

/// Parse for loop parameters: -> $param or -> $a, $b
fn parse_for_params(input: &str) -> PResult<'_, (Option<String>, Vec<String>)> {
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

fn for_stmt(input: &str) -> PResult<'_, Stmt> {
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

fn parse_pointy_param(input: &str) -> PResult<'_, String> {
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
fn while_stmt(input: &str) -> PResult<'_, Stmt> {
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
fn until_stmt(input: &str) -> PResult<'_, Stmt> {
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
fn loop_stmt(input: &str) -> PResult<'_, Stmt> {
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
fn repeat_stmt(input: &str) -> PResult<'_, Stmt> {
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
fn given_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("given", input).ok_or_else(|| PError::expected("given statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, topic) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Given { topic, body }))
}

fn when_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("when", input).ok_or_else(|| PError::expected("when statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}

/// Parse `sub` declaration.
fn sub_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r).unwrap_or(r);
        let (r, _) = ws(r)?;
        (r, true)
    } else {
        let r = keyword("sub", input).ok_or_else(|| PError::expected("sub declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    sub_decl_body(rest, multi)
}

fn sub_decl_body(input: &str, multi: bool) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    let (rest, _) = ws(rest)?;

    // Parse params
    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::SubDecl {
            name,
            params,
            param_defs,
            body,
            multi,
        },
    ))
}

/// Parse parameter list inside parens.
fn parse_param_list(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let mut params = Vec::new();
    let mut rest = input;
    if rest.starts_with(')') {
        return Ok((rest, params));
    }
    let (r, p) = parse_single_param(rest)?;
    params.push(p);
    rest = r;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, params));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, params));
        }
        let (r, p) = parse_single_param(r)?;
        params.push(p);
        rest = r;
    }
}

fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Capture-all: (|) or (|$c)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        // Optional capture variable name
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = var_name(r)?;
            return Ok((
                r,
                ParamDef {
                    name,
                    named: false,
                    slurpy: true,
                    default: None,
                    type_constraint: None,
                    literal_value: None,
                },
            ));
        }
        // Bare |
        return Ok((
            r,
            ParamDef {
                name: "_capture".to_string(),
                named: false,
                slurpy: true,
                default: None,
                type_constraint: None,
                literal_value: None,
            },
        ));
    }

    // Slurpy: *@arr or *%hash
    if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@' || rest.as_bytes()[1] == b'%' || rest.as_bytes()[1] == b'$')
    {
        slurpy = true;
        rest = &rest[1..];
    }

    // Named param marker: :$name
    if rest.starts_with(':') {
        named = true;
        rest = &rest[1..];
    }

    // Type constraint
    if let Ok((r, tc)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') || r2.starts_with(':')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        }
    }

    let (rest, name) = var_name(rest)?;
    let (rest, _) = ws(rest)?;

    // Default value
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // `is copy`, `is rw`, `is readonly` traits
    let (rest, _) = ws(rest)?;
    let rest = if let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, _trait) = ident(r)?;
        r
    } else {
        rest
    };

    // `where` constraint
    let (rest, _) = ws(rest)?;
    let rest = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, _constraint) = expression(r)?;
        r
    } else {
        rest
    };

    Ok((
        rest,
        ParamDef {
            name,
            default,
            named,
            slurpy,
            type_constraint,
            literal_value: None,
        },
    ))
}

/// Parse `method` declaration.
fn method_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("method", r).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = keyword("method", input).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body(rest, multi)
}

fn method_decl_body(input: &str, multi: bool) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    let (rest, _) = ws(rest)?;

    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::MethodDecl {
            name,
            params,
            param_defs,
            body,
            multi,
        },
    ))
}

/// Parse `class` declaration.
fn class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("class declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;

    // Parent classes: is Parent
    let mut parents = Vec::new();
    let mut r = rest;
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent) = qualified_ident(r2)?;
        parents.push(parent);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    // does Role
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        // Treat "does" as parent for now
        parents.push(role_name);
        let (r2, _) = ws(r2)?;
        r = r2;
    }

    let (rest, body) = block(r)?;
    Ok((
        rest,
        Stmt::ClassDecl {
            name,
            parents,
            body,
        },
    ))
}

/// Parse `role` declaration.
fn role_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::RoleDecl { name, body }))
}

/// Parse `has` attribute declaration.
fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // Optional type constraint
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

    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, _) = if sigil == b'$' || sigil == b'@' || sigil == b'%' {
        (&rest[1..], ())
    } else {
        return Err(PError::expected("sigil ($, @, %)"));
    };

    // Check for public accessor marker '.'
    let (rest, is_public) = if let Some(stripped) = rest.strip_prefix('.') {
        (stripped, true)
    } else if let Some(stripped) = rest.strip_prefix('!') {
        (stripped, false)
    } else {
        (rest, false)
    };

    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();
    let (rest, _) = ws(rest)?;

    // Default value
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // `is` traits
    let (rest, _) = ws(rest)?;
    let rest = if let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, _trait_name) = ident(r)?;
        r
    } else {
        rest
    };

    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name,
            is_public,
            default,
        },
    ))
}

/// Parse `enum` declaration.
fn enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("enum", input).ok_or_else(|| PError::expected("enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    parse_enum_decl_body(rest)
}

fn parse_enum_decl_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    let (rest, _) = ws(rest)?;

    // Enum variants in <> or ()
    let (rest, variants) = if rest.starts_with('<') {
        let (r, _) = parse_char(rest, '<')?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            let (r2, _) = take_while_opt(r, |c: char| c == ' ' || c == '\t');
            if let Some(r2) = r2.strip_prefix('>') {
                r = r2;
                break;
            }
            let (r2, word) =
                take_while1(r2, |c: char| c != '>' && c != ' ' && c != '\t' && c != '\n')?;
            variants.push((word.to_string(), None));
            r = r2;
        }
        (r, variants)
    } else if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            if let Some(r) = r.strip_prefix(')') {
                return Ok((r, Stmt::EnumDecl { name, variants }));
            }
            let (r2, vname) = ident(r)?;
            let (r2, _) = ws(r2)?;
            let (r2, val) = if let Some(stripped) = r2.strip_prefix("=>") {
                let (r2, _) = ws(stripped)?;
                let (r2, expr) = expression(r2)?;
                (r2, Some(expr))
            } else {
                (r2, None)
            };
            variants.push((vname, val));
            let (r2, _) = ws(r2)?;
            if let Some(stripped) = r2.strip_prefix(',') {
                let (r2, _) = ws(stripped)?;
                r = r2;
            } else {
                r = r2;
            }
        }
    } else {
        (rest, Vec::new())
    };

    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::EnumDecl { name, variants }))
}

/// Parse `return` statement.
fn return_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("return", input).ok_or_else(|| PError::expected("return statement"))?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Return(Expr::Literal(Value::Nil))));
    }
    let (rest, expr) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Return(expr)))
}

/// Parse `last` / `next` / `redo`.
fn last_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("last", input).ok_or_else(|| PError::expected("last statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Last(None)))
}

fn next_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("next", input).ok_or_else(|| PError::expected("next statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Next(None)))
}

fn redo_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("redo", input).ok_or_else(|| PError::expected("redo statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Redo(None)))
}

/// Parse `die` statement.
fn die_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("die", input)
        .or_else(|| keyword("fail", input))
        .ok_or_else(|| PError::expected("die/fail statement"))?;
    let (rest, _) = ws(rest)?;
    if rest.starts_with(';') || rest.is_empty() || rest.starts_with('}') {
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::Die(Expr::Literal(Value::Str("Died".to_string()))),
        ));
    }
    let (rest, expr) = expression(rest)?;
    let stmt = Stmt::Die(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse `take` statement.
fn take_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("take", input).ok_or_else(|| PError::expected("take statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, expr) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Take(expr)))
}

/// Parse CATCH/CONTROL blocks.
fn catch_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CATCH", input).ok_or_else(|| PError::expected("CATCH block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Catch(body)))
}

fn control_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("CONTROL", input).ok_or_else(|| PError::expected("CONTROL block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Control(body)))
}

/// Parse phasers: BEGIN, END, etc.
fn phaser_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, kind) = if let Some(r) = keyword("BEGIN", input) {
        (r, PhaserKind::Begin)
    } else if let Some(r) = keyword("CHECK", input) {
        (r, PhaserKind::Begin)
    } else if let Some(r) = keyword("INIT", input) {
        (r, PhaserKind::Begin)
    } else if let Some(r) = keyword("END", input) {
        (r, PhaserKind::End)
    } else if let Some(r) = keyword("ENTER", input) {
        (r, PhaserKind::Enter)
    } else if let Some(r) = keyword("LEAVE", input) {
        (r, PhaserKind::Leave)
    } else if let Some(r) = keyword("FIRST", input) {
        (r, PhaserKind::First)
    } else if let Some(r) = keyword("NEXT", input) {
        (r, PhaserKind::Next)
    } else if let Some(r) = keyword("LAST", input) {
        (r, PhaserKind::Last)
    } else {
        return Err(PError::expected("phaser keyword"));
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = if rest.starts_with('{') {
        block(rest)?
    } else {
        let (r, s) = statement(rest)?;
        (r, vec![s])
    };
    Ok((rest, Stmt::Phaser { kind, body }))
}

/// Parse `does` declaration.
fn does_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("does", input).ok_or_else(|| PError::expected("does declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::DoesDecl { name }))
}

/// Parse `subtest` declaration.
fn subtest_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subtest", input).ok_or_else(|| PError::expected("subtest statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    // Expect =>
    if !rest.starts_with("=>") {
        return Err(PError::expected("'=>' in subtest"));
    }
    let rest = &rest[2..];
    let (rest, _) = ws(rest)?;
    // Optional 'sub' keyword
    let rest = if let Some(r) = keyword("sub", rest) {
        let (r, _) = ws(r)?;
        r
    } else {
        rest
    };
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::Subtest { name, body }))
}

/// Parse `react` block.
fn react_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("react", input).ok_or_else(|| PError::expected("react block"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::React { body }))
}

/// Parse `whenever` block.
fn whenever_stmt(input: &str) -> PResult<'_, Stmt> {
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

/// Parse `constant` declaration.
fn constant_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("constant", input).ok_or_else(|| PError::expected("constant declaration"))?;
    let (rest, _) = ws1(rest)?;
    // The name can be $var or bare identifier
    let (rest, name) = if rest.starts_with('$') {
        let (r, n) = var_name(rest)?;
        (r, format!("${}", n))
    } else {
        ident(rest)?
    };
    let (rest, _) = ws(rest)?;
    if rest.starts_with('=') || rest.starts_with(":=") {
        let rest = if let Some(stripped) = rest.strip_prefix(":=") {
            stripped
        } else {
            &rest[1..]
        };
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        return Ok((
            rest,
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::VarDecl {
            name,
            expr: Expr::Literal(Value::Nil),
            type_constraint: None,
        },
    ))
}

/// Parse `subset` declaration.
fn subset_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("subset", input).ok_or_else(|| PError::expected("subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, base) = if let Some(r) = keyword("of", rest) {
        let (r, _) = ws1(r)?;
        let (r, base) = ident(r)?;
        let (r, _) = ws(r)?;
        (r, base)
    } else {
        (rest, "Any".to_string())
    };
    let rest =
        keyword("where", rest).ok_or_else(|| PError::expected("'where' in subset declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, predicate) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::SubsetDecl {
            name,
            base,
            predicate,
        },
    ))
}

/// Parse statement modifier (postfix if/unless/for/while/until/given/when).
fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;

    // If there's a semicolon, the statement is terminated — no modifiers
    if let Some(stripped) = rest.strip_prefix(';') {
        return Ok((stripped, stmt));
    }

    // If at end of input or block, return as-is
    if rest.is_empty() || rest.starts_with('}') {
        return Ok((rest, stmt));
    }

    // Try statement modifiers
    if let Some(r) = keyword("if", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::If {
                cond,
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            },
        ));
    }
    if let Some(r) = keyword("unless", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            },
        ));
    }
    if let Some(r) = keyword("for", rest) {
        let (r, _) = ws1(r)?;
        let (r, iterable) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::For {
                iterable,
                param: None,
                params: Vec::new(),
                body: vec![stmt],
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::While {
                cond,
                body: vec![stmt],
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
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![stmt],
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("given", rest) {
        let (r, _) = ws1(r)?;
        let (r, topic) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Given {
                topic,
                body: vec![stmt],
            },
        ));
    }

    Ok((rest, stmt))
}

/// Known function names that get Stmt::Call treatment at statement level.
const KNOWN_CALLS: &[&str] = &[
    "ok",
    "is",
    "isnt",
    "nok",
    "pass",
    "flunk",
    "cmp-ok",
    "like",
    "unlike",
    "is-deeply",
    "is-approx",
    "isa-ok",
    "lives-ok",
    "dies-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "is_run",
    "throws-like",
    "force_todo",
    "force-todo",
    "plan",
    "done-testing",
    "bail-out",
    "skip",
    "skip-rest",
    "diag",
    "todo",
    "does-ok",
    "can-ok",
    "use-ok",
    "dd",
    "exit",
    "proceed",
    "succeed",
    "push",
    "pop",
    "shift",
    "unshift",
    "append",
    "prepend",
    "elems",
    "chars",
    "defined",
    "warn",
];

/// Check if a name is a known statement-level function call.
fn is_known_call(name: &str) -> bool {
    KNOWN_CALLS.contains(&name)
}

/// Parse a known function call as statement.
fn known_call_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
    if !is_known_call(&name) {
        return Err(PError::expected("known function call"));
    }
    let had_ws = rest.starts_with(' ') || rest.starts_with('\t') || rest.starts_with('\n');
    let (rest, _) = ws(rest)?;

    // Special handling for proceed/succeed with no args
    if name == "proceed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Proceed));
    }
    if name == "succeed" {
        let (rest, _) = opt_char(rest, ';');
        return Ok((rest, Stmt::Succeed));
    }

    // In Raku, `foo(args)` (no space) = paren call, but `foo (expr)` (space) = listop call.
    // When there was whitespace before `(`, treat `(` as expression grouping, not call parens.
    let (rest, args) = if had_ws {
        parse_stmt_call_args_no_paren(rest)?
    } else {
        parse_stmt_call_args(rest)?
    };
    let stmt = Stmt::Call { name, args };
    parse_statement_modifier(rest, stmt)
}

/// Parse assignment statement: $x = expr, @arr = expr, etc.
fn assign_stmt(input: &str) -> PResult<'_, Stmt> {
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    if sigil != b'$' && sigil != b'@' && sigil != b'%' {
        return Err(PError::expected("assignment"));
    }

    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        _ => "",
    };

    let (rest, var) = var_name(input)?;
    let name = format!("{}{}", prefix, var);
    let (rest, _) = ws(rest)?;

    // Compound assignment: +=, -=, ~=, *=, //=, ||=, &&=
    let compound_ops: &[(&str, TokenKind)] = &[
        ("+=", TokenKind::Plus),
        ("-=", TokenKind::Minus),
        ("~=", TokenKind::Tilde),
        ("*=", TokenKind::Star),
        ("//=", TokenKind::SlashSlash),
        ("||=", TokenKind::OrOr),
        ("&&=", TokenKind::AndAnd),
    ];
    for &(op_str, ref op_kind) in compound_ops {
        if let Some(stripped) = rest.strip_prefix(op_str) {
            let (rest, _) = ws(stripped)?;
            let (rest, rhs) = parse_assign_expr_or_comma(rest)?;
            let expr = Expr::Binary {
                left: Box::new(Expr::Var(name.clone())),
                op: op_kind.clone(),
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

    // Mutating method call: $x.=method or $x .= method(args)
    if let Some(stripped) = rest.strip_prefix(".=") {
        let (stripped, _) = ws(stripped)?;
        let (r, method_name) = take_while1(stripped, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        let method_name = method_name.to_string();
        let (r, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, a) = super::primary::parse_call_arg_list(r)?;
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
        };
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(r, stmt);
    }

    // Simple assignment
    if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = parse_assign_expr_or_comma(rest)?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
        };
        return parse_statement_modifier(rest, stmt);
    }
    // Binding
    if let Some(stripped) = rest.strip_prefix(":=") {
        let (rest, _) = ws(stripped)?;
        let (rest, expr) = parse_comma_or_expr(rest)?;
        let stmt = Stmt::Assign {
            name,
            expr,
            op: AssignOp::Bind,
        };
        return parse_statement_modifier(rest, stmt);
    }

    Err(PError::expected("assignment"))
}

/// Parse a block statement: { ... }
fn block_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, body) = block(input)?;
    Ok((rest, Stmt::Block(body)))
}

/// Parse an expression statement (fallback).
fn expr_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, expr) = expression(input)?;

    // Check for index assignment after expression
    let (rest, _) = ws(rest)?;
    if matches!(expr, Expr::Index { .. }) && rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, value) = parse_comma_or_expr(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = opt_char(rest, ';');
        if let Expr::Index { target, index } = expr {
            return Ok((
                rest,
                Stmt::Expr(Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(value),
                }),
            ));
        }
    }

    let stmt = Stmt::Expr(expr);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `token` or `rule` declaration.
fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let is_rule = keyword("rule", input).is_some();
    let rest = keyword("token", input)
        .or_else(|| keyword("rule", input))
        .ok_or_else(|| PError::expected("token/rule declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;

    // Optional params
    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;

    if is_rule {
        Ok((
            rest,
            Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    } else {
        Ok((
            rest,
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    }
}

/// Parse `grammar` declaration.
fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("grammar declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `unit module` statement.
fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("module", rest).ok_or_else(|| PError::expected("'module' after 'unit'"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Package {
            name,
            body: Vec::new(),
        },
    ))
}

/// Parse `package` declaration.
fn package_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `proto` declaration.
fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto sub | proto method
    let _is_token = keyword("token", rest).is_some() || keyword("rule", rest).is_some();
    let rest = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("sub", rest))
        .or_else(|| keyword("method", rest))
        .unwrap_or(rest);
    let (rest, _) = ws1(rest)?;
    let (rest, name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_defs) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, pd)
    } else {
        (rest, Vec::new())
    };
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    let (rest, _) = ws(rest)?;
    // May have {*} body or just semicolon
    if rest.starts_with('{') {
        let (rest, _body) = block(rest)?;
        return Ok((
            rest,
            Stmt::ProtoDecl {
                name,
                params,
                param_defs,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::ProtoDecl {
            name,
            params,
            param_defs,
        },
    ))
}

/// Parse `with`/`without` statement.
fn with_stmt(input: &str) -> PResult<'_, Stmt> {
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

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch: body,
            else_branch: Vec::new(),
        },
    ))
}

/// Parse a single statement.
fn statement(input: &str) -> PResult<'_, Stmt> {
    let (input, _) = ws(input)?;

    // Try each statement form in order
    if let Ok(r) = use_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = unit_module_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = my_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = constant_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = class_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = role_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = grammar_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = subset_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = enum_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = has_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = does_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = proto_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = sub_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = method_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = token_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = say_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = print_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = note_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = if_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = unless_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = with_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = labeled_loop_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = for_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = while_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = until_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = loop_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = repeat_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = given_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = when_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = default_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = return_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = last_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = next_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = redo_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = die_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = take_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = catch_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = control_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = phaser_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = subtest_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = react_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = whenever_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = package_decl(input) {
        return Ok(r);
    }
    if let Ok(r) = known_call_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = assign_stmt(input) {
        return Ok(r);
    }
    if let Ok(r) = block_stmt(input) {
        return Ok(r);
    }
    expr_stmt(input)
}

/// Parse a full program (sequence of statements).
pub(super) fn program(input: &str) -> PResult<'_, Vec<Stmt>> {
    // Strip BOM if present
    let input = if let Some(stripped) = input.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        input
    };
    stmt_list(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_use_test() {
        let (rest, stmts) = program("use Test;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::Use { module, .. } if module == "Test"));
    }

    #[test]
    fn parse_plan_call() {
        let (rest, stmts) = program("plan 1;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::Call { name, .. } if name == "plan"));
    }

    #[test]
    fn parse_ok_call_with_named() {
        let (rest, stmts) = program("ok 0, :todo(1);").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        if let Stmt::Call { name, args } = &stmts[0] {
            assert_eq!(name, "ok");
            assert_eq!(args.len(), 2);
            assert!(matches!(&args[1], CallArg::Named { name, .. } if name == "todo"));
        } else {
            panic!("Expected Call");
        }
    }

    #[test]
    fn parse_my_var_decl() {
        let (rest, stmts) = program("my $x = '0';").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { name, .. } if name == "x"));
    }

    #[test]
    fn parse_plan_skip_all() {
        let (rest, stmts) = program("plan skip-all => \"msg\";").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        if let Stmt::Call { name, args } = &stmts[0] {
            assert_eq!(name, "plan");
            assert!(matches!(&args[0], CallArg::Named { name, .. } if name == "skip-all"));
        } else {
            panic!("Expected Call");
        }
    }

    #[test]
    fn parse_basic_test_program() {
        let input = "use Test;\nplan 1;\nok 1, 'test';";
        let (rest, stmts) = program(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 3);
    }

    #[test]
    fn parse_if_else() {
        let (rest, stmts) = program("if $x { say 1; } else { say 2; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::If { .. }));
    }

    #[test]
    fn parse_for_loop() {
        let (rest, stmts) = program("for 1..10 -> $i { say $i; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_sub_decl() {
        let (rest, stmts) = program("sub foo($x) { return $x; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::SubDecl { name, .. } if name == "foo"));
    }

    #[test]
    fn parse_class_decl() {
        let (rest, stmts) = program("class Foo { has $.x; method bar { 42 } }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::ClassDecl { name, .. } if name == "Foo"));
    }

    #[test]
    fn parse_my_no_space() {
        let (rest, stmts) = program("my $a=0; say $a").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2, "stmts: {:?}", stmts);
        if let Stmt::VarDecl { name, expr, .. } = &stmts[0] {
            assert_eq!(name, "a");
            assert!(
                matches!(expr, Expr::Literal(Value::Int(0))),
                "Expected Int(0), got {:?}",
                expr
            );
        } else {
            panic!("Expected VarDecl, got {:?}", stmts[0]);
        }
    }
}
