use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

/// Parse a user-declared circumfix operator: `open semilist close` → Call
/// `circumfix:<open close>(semilist)`.
///
/// Like rakudo, the operator receives its whole semilist as ONE positional
/// argument: `⦃ 1, 2 ⦄` passes the List `(1, 2)`, `⦃ ⦄` the empty List, and a
/// lone `⦃ a => 1 ⦄` the Pair itself -- as data, never as a named argument.
pub(crate) fn declared_circumfix_op(input: &str) -> PResult<'_, Expr> {
    let Some((name, open_len, close_delim)) =
        crate::parser::stmt::simple::match_user_declared_circumfix_op(input)
    else {
        return Err(PError::expected("declared circumfix operator"));
    };
    let open = &input[..open_len];
    let (mut rest, _) = ws(&input[open_len..])?;
    let mut items = Vec::new();
    let mut saw_comma = false;
    let after = loop {
        if let Some(after) = rest.strip_prefix(close_delim.as_str()) {
            break after;
        }
        let (r, item) = expression(rest)?;
        items.push(item);
        let (r, _) = ws(r)?;
        if let Some(after_comma) = r.strip_prefix(',') {
            saw_comma = true;
            let (r, _) = ws(after_comma)?;
            rest = r;
            continue;
        }
        match r.strip_prefix(close_delim.as_str()) {
            Some(after) => break after,
            None => return Err(circumfix_fail_goal(open, &close_delim, r)),
        }
    };
    let arg = match items.pop() {
        Some(item) if items.is_empty() && !saw_comma => positional_operand(item),
        last => {
            items.extend(last);
            Expr::ArrayLiteral(items)
        }
    };
    Ok((
        after,
        Expr::Call {
            name: Symbol::intern(&name),
            args: vec![arg],
        },
    ))
}

/// An operator's operand is always positional: rakudo compiles
/// `⦃ a => 1 ⦄` to `&circumfix:<⦃ ⦄>(a => 1)` with the Pair as *data*, never
/// as a named argument (only a call's own argument list turns `=>` into a
/// named). So a pair-shaped operand is marked positional here, exactly like a
/// parenthesized `(a => 1)` argument.
pub(crate) fn positional_operand(arg: Expr) -> Expr {
    let is_pair = match &arg {
        Expr::Binary { op, .. } => *op == crate::token_kind::TokenKind::FatArrow,
        Expr::Literal(lit) => matches!(lit.view(), crate::value::ValueView::Pair(..)),
        _ => false,
    };
    if is_pair {
        Expr::PositionalPair(Box::new(arg))
    } else {
        arg
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
