use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

/// Parse a user-declared circumfix operator: `open args close` → Call circumfix:<open close>(args)
pub(crate) fn declared_circumfix_op(input: &str) -> PResult<'_, Expr> {
    if let Some((name, open_len, close_delim)) =
        crate::parser::stmt::simple::match_user_declared_circumfix_op(input)
    {
        let open = &input[..open_len];
        let rest = &input[open_len..];
        let (rest, _) = ws(rest)?;
        // Check for empty circumfix: `open close` with nothing inside
        if let Some(after) = rest.strip_prefix(close_delim.as_str()) {
            return Ok((
                after,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args: vec![],
                },
            ));
        }
        // Parse first argument
        let (r, arg) = expression(rest)?;
        let args = vec![arg];
        let (r, _) = ws(r)?;
        // Check if more args follow (comma-separated)
        if let Some(after_comma) = r.strip_prefix(',') {
            let (r, _) = ws(after_comma)?;
            return parse_circumfix_rest(r, open, &close_delim, name, args);
        }
        // Check for closing delimiter
        if let Some(after) = r.strip_prefix(close_delim.as_str()) {
            return Ok((
                after,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args,
                },
            ));
        }
        return Err(circumfix_fail_goal(open, &close_delim, r));
    }
    Err(PError::expected("declared circumfix operator"))
}

fn parse_circumfix_rest<'a>(
    mut rest: &'a str,
    open: &str,
    close_delim: &str,
    name: String,
    mut args: Vec<Expr>,
) -> PResult<'a, Expr> {
    loop {
        if let Some(after) = rest.strip_prefix(close_delim) {
            return Ok((
                after,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args,
                },
            ));
        }
        let (r, arg) = expression(rest)?;
        args.push(arg);
        let (r, _) = ws(r)?;
        if let Some(after_comma) = r.strip_prefix(',') {
            let (r, _) = ws(after_comma)?;
            rest = r;
            continue;
        }
        let (r, _) = ws(r)?;
        if let Some(after) = r.strip_prefix(close_delim) {
            return Ok((
                after,
                Expr::Call {
                    name: Symbol::intern(&name),
                    args,
                },
            ));
        }
        return Err(circumfix_fail_goal(open, close_delim, r));
    }
}

/// An in-scope custom `circumfix:<open close>` operator's opener matched and its
/// argument(s) parsed, but the closing delimiter is missing (e.g. `⟨5;`). The
/// bracket is committed, so this is a hard parse failure — X::Comp::FailGoal
/// carrying the operator's `dba` (`circumfix:sym<open close>`) and its `goal`.
fn circumfix_fail_goal(open: &str, close_delim: &str, pos: &str) -> PError {
    let dba = format!("circumfix:sym<{} {}>", open, close_delim);
    let goal = format!("'{}'", close_delim);
    crate::parser::primary::fail_goal_error_at(&dba, &goal, Some(pos))
}

pub(crate) fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) =
        crate::parser::primary::regex::scan_to_delim(after_open, '{', '}', true)
    {
        return Ok((rest, body.trim().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}
