use super::super::parse_result::{PError, PResult, parse_char, take_while1};

use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::value::Value;

use super::super::expr::expression;
use super::super::helpers::ws;
use super::super::stmt::keyword;
use super::string::{double_quoted_string, single_quoted_string};
use super::var::parse_ident_with_hyphens;

use std::sync::atomic::{AtomicU64, Ordering};

fn skip_pointy_return_type(mut r: &str) -> PResult<'_, ()> {
    let (r2, _) = ws(r)?;
    r = r2;
    if let Some(after_arrow) = r.strip_prefix("-->") {
        let (after_arrow, _) = ws(after_arrow)?;
        // Keep this permissive for now: simple type-like names only.
        let (after_arrow, _type_name) = take_while1(after_arrow, |c: char| {
            c.is_alphanumeric() || c == '_' || c == ':'
        })?;
        let (after_arrow, _) = ws(after_arrow)?;
        Ok((after_arrow, ()))
    } else {
        Ok((r, ()))
    }
}

/// Known reduction operators (must be listed to distinguish from array literals).
const REDUCTION_OPS: &[&str] = &[
    "+", "-", "*", "/", "~", "||", "&&", "//", "%%", "**", "+&", "+|", "+^", "?&", "?|", "?^",
    "==", "!=", "<", ">", "<=", ">=", "<=>", "===", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", ",", "after", "before",
];

/// Parse a reduction operator: [+], [*], [~], [min], [max], [gcd], [lcm], [||], [&&], etc.
pub(super) fn reduction_op(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    let r = &input[1..];
    // Find the closing ]
    let end = r
        .find(']')
        .ok_or_else(|| PError::expected("']' closing reduction"))?;
    let op = &r[..end];
    if op.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    // Only accept known operators to avoid confusion with array literals
    // Also support negated operators like [!after], [!before], [!==], etc.
    let base_op = op.strip_prefix('!').unwrap_or(op);
    if !REDUCTION_OPS.contains(&base_op) {
        return Err(PError::expected("known reduction operator"));
    }
    let r = &r[end + 1..];
    // Must be followed by whitespace and an expression (not just `]`)
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') || r.starts_with(')') {
        return Err(PError::expected("expression after reduction operator"));
    }
    let (r, _) = ws(r)?;
    // Parse comma-separated list as the operand
    let (r, first) = expression(r)?;
    let mut items = vec![first];
    let mut rest = r;
    loop {
        let (r, _) = ws_inner(rest);
        if !r.starts_with(',') {
            break;
        }
        let r = &r[1..];
        let (r, _) = ws_inner(r);
        // Stop at end-of-input, semicolon, closing brackets, or statement modifiers
        if r.is_empty()
            || r.starts_with(';')
            || r.starts_with('}')
            || r.starts_with(')')
            || r.starts_with(']')
        {
            rest = r;
            break;
        }
        if let Ok((r, next)) = expression(r) {
            items.push(next);
            rest = r;
        } else {
            return Err(PError::expected_at(
                "expression after ',' in reduction list",
                r,
            ));
        }
    }
    let expr = if items.len() == 1 {
        items.remove(0)
    } else {
        Expr::ArrayLiteral(items)
    };
    Ok((
        rest,
        Expr::Reduction {
            op: op.to_string(),
            expr: Box::new(expr),
        },
    ))
}

/// Parse colonpair expressions: :$var, :@var, :%var, :name(expr), :name, :!name
pub(in crate::parser) fn colonpair_expr(input: &str) -> PResult<'_, Expr> {
    let r = input
        .strip_prefix(':')
        .filter(|r| !r.starts_with(':'))
        .ok_or_else(|| PError::expected("colonpair"))?;
    // :!name (negated boolean pair)
    if let Some(after_bang) = r.strip_prefix('!') {
        let (rest, name) = parse_ident_with_hyphens(after_bang)?;
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            },
        ));
    }
    // :$var / :@var / :%var (autopair from variable)
    if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
        let sigil = &r[..1];
        let after_sigil = &r[1..];
        let (rest, var_name) = parse_ident_with_hyphens(after_sigil)?;
        let var_expr = match sigil {
            "$" => Expr::Var(var_name.to_string()),
            "@" => Expr::ArrayVar(var_name.to_string()),
            "%" => Expr::HashVar(var_name.to_string()),
            _ => unreachable!(),
        };
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(var_name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(var_expr),
            },
        ));
    }
    // :name(expr) or :name (boolean true pair)
    let (rest, name) = parse_ident_with_hyphens(r)?;
    // Don't parse statement-modifier keywords as colonpairs
    if matches!(
        name,
        "if" | "unless" | "for" | "while" | "until" | "given" | "when"
    ) {
        return Err(PError::expected("colonpair name"));
    }
    if let Some(after_paren) = rest.strip_prefix('(') {
        let (r, _) = ws(after_paren)?;
        let (r, val) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(val),
            },
        ));
    }
    // :name{ ... } (block-valued colonpair)
    if rest.starts_with('{') {
        let (r, body) = parse_block_body(rest)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::AnonSub(body)),
            },
        ));
    }
    // :name<value> (angle-bracket colonpair, equivalent to :name("value"))
    if rest.starts_with('<') && !rest.starts_with("<<") && !rest.starts_with("<=") {
        let inner = &rest[1..];
        if let Some(close) = inner.find('>') {
            let content = &inner[..close];
            let r = &inner[close + 1..];
            let words: Vec<&str> = content.split_whitespace().collect();
            if !words.is_empty() {
                let val_expr = if words.len() == 1 {
                    Expr::Literal(Value::Str(words[0].to_string()))
                } else {
                    Expr::ArrayLiteral(
                        words
                            .into_iter()
                            .map(|w| Expr::Literal(Value::Str(w.to_string())))
                            .collect(),
                    )
                };
                return Ok((
                    r,
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
                        op: crate::token_kind::TokenKind::FatArrow,
                        right: Box::new(val_expr),
                    },
                ));
            }
        }
    }
    // :name (boolean true)
    Ok((
        rest,
        Expr::Binary {
            left: Box::new(Expr::Literal(Value::Str(name.to_string()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Bool(true))),
        },
    ))
}

/// Parse `\(...)` Capture literal.
pub(super) fn capture_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("\\(") {
        return Err(PError::expected("capture literal"));
    }
    let r = &input[1..]; // skip backslash, keep (
    let (r, _) = parse_char(r, '(')?;
    let (r, _) = ws(r)?;
    let (r, items) = super::parse_call_arg_list(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, ')')?;
    // Represent as ArrayLiteral for now (Capture is not in the AST)
    Ok((r, Expr::ArrayLiteral(items)))
}

/// Parse `-> $param { body }` or `-> $a, $b { body }` arrow lambda.
pub(super) fn arrow_lambda(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("->") {
        return Err(PError::expected("arrow lambda"));
    }
    let r = &input[2..];
    let (r, _) = ws(r)?;
    // Zero-param pointed block: -> { body }
    if r.starts_with('{') {
        let (r, body) = parse_block_body(r)?;
        return Ok((r, Expr::AnonSub(body)));
    }
    // Sub-signature destructuring: -> (:key($var), :value($var2)) { body }
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = super::super::stmt::parse_param_list_pub(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        let params: Vec<String> = sub_params.iter().map(|p| p.name.clone()).collect();
        return Ok((r, Expr::AnonSubParams { params, body }));
    }
    // Parse params
    let (r, first) = super::super::stmt::parse_pointy_param_pub(r)?;
    let (r, _) = ws(r)?;
    if r.starts_with(',') {
        // Multi-param: -> $a, $b { body }
        let mut params = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            let (r2, next) = super::super::stmt::parse_pointy_param_pub(r2)?;
            params.push(next);
            let (r2, _) = ws(r2)?;
            if !r2.starts_with(',') {
                r = r2;
                break;
            }
            r = r2;
        }
        let (r, _) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::AnonSubParams { params, body }))
    } else {
        // Single param: -> $n { body }
        let (r, _) = skip_pointy_return_type(r)?;
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::Lambda { param: first, body }))
    }
}

/// Parse a block `{ stmts }` as AnonSub or `{}` / `{ key => val, ... }` as Hash.
pub(super) fn block_or_hash_expr(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('{') {
        return Err(PError::expected("block or hash"));
    }
    let r = &input[1..];
    let (r, _) = ws_inner(r);

    // Empty hash: {}
    if let Some(rest) = r.strip_prefix('}') {
        return Ok((rest, Expr::Hash(Vec::new())));
    }

    // Try to detect if this is a hash literal: { key => val, ... }
    // Heuristic: if after ws we see `ident =>` or `"str" =>` or `'str' =>`, it's a hash
    if is_hash_literal_start(r) {
        return parse_hash_literal_body(r);
    }

    // Otherwise parse as a block (anonymous sub)
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Expr> {
        let (r, stmts) = super::super::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        if !r.starts_with('}') {
            return Err(PError::expected("'}'"));
        }
        let r = &r[1..];
        Ok((r, make_anon_sub(stmts)))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Simple whitespace consumer that doesn't use PResult (infallible).
pub(super) fn ws_inner(input: &str) -> (&str, ()) {
    match super::super::helpers::ws(input) {
        Ok((r, _)) => (r, ()),
        Err(_) => (input, ()),
    }
}

/// Parse a block body: { stmts }
pub(in crate::parser) fn parse_block_body(input: &str) -> PResult<'_, Vec<crate::ast::Stmt>> {
    let (r, _) = parse_char(input, '{')?;
    crate::parser::stmt::simple::push_scope();
    let result = (|| -> PResult<'_, Vec<crate::ast::Stmt>> {
        let (r, stmts) = super::super::stmt::stmt_list_pub(r)?;
        let (r, _) = ws_inner(r);
        let (r, _) = parse_char(r, '}')?;
        Ok((r, stmts))
    })();
    crate::parser::stmt::simple::pop_scope();
    result
}

/// Check if the input looks like a hash literal start.
fn is_hash_literal_start(input: &str) -> bool {
    // ident => or "str" => or 'str' =>
    if let Ok((r, _)) = super::super::stmt::ident_pub(input) {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // Quoted key => val
    if (input.starts_with('"') || input.starts_with('\''))
        && let Ok((r, _)) = single_quoted_string(input).or_else(|_| double_quoted_string(input))
    {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    false
}

static ANON_CLASS_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Parse an anonymous class expression: `class { ... }`
pub(super) fn anon_class_expr(input: &str) -> PResult<'_, Expr> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("anonymous class"))?;
    let (rest, _) = ws(rest)?;
    // Must be followed by '{' (no name) to be an anonymous class
    if !rest.starts_with('{') {
        return Err(PError::expected("'{' for anonymous class"));
    }
    let id = ANON_CLASS_COUNTER.fetch_add(1, Ordering::Relaxed);
    let name = format!("__ANON_CLASS_{id}__");
    let (rest, body) = parse_block_body(rest)?;
    Ok((
        rest,
        Expr::DoStmt(Box::new(Stmt::ClassDecl {
            name,
            parents: Vec::new(),
            body,
        })),
    ))
}

/// Parse hash literal body: key => val, key => val, ... }
fn parse_hash_literal_body(input: &str) -> PResult<'_, Expr> {
    let mut pairs = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            return Ok((rest, Expr::Hash(pairs)));
        }
        // Parse key as identifier or string
        let (r, key) = if let Ok((r, name)) = super::super::stmt::ident_pub(r) {
            (r, name)
        } else if let Ok((r, Expr::Literal(Value::Str(s)))) =
            single_quoted_string(r).or_else(|_| double_quoted_string(r))
        {
            (r, s)
        } else {
            return Err(PError::expected("hash key"));
        };
        let (r, _) = ws_inner(r);
        // Expect =>
        if !r.starts_with("=>") {
            return Err(PError::expected("'=>' in hash literal"));
        }
        let r = &r[2..];
        let (r, _) = ws_inner(r);
        let (r, val) = super::super::expr::expression(r)?;
        pairs.push((key, Some(val)));
        let (r, _) = ws_inner(r);
        if let Some(stripped) = r.strip_prefix(',') {
            rest = stripped;
        } else {
            rest = r;
        }
    }
}
