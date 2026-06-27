use super::*;
use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;

/// Parse qx{...}, qqx{...} forms.
/// qx executes with single-quote (backslash) interpolation.
/// qqx executes with full interpolation.
pub(crate) fn qx_string(input: &str) -> PResult<'_, Expr> {
    // Try qqx first, then qx
    let (after_qx, is_qq) = if let Some(r) = input.strip_prefix("qqx") {
        (r, true)
    } else if let Some(r) = input.strip_prefix("qx") {
        (r, false)
    } else {
        return Err(PError::expected("qx string"));
    };
    let delim = after_qx
        .chars()
        .next()
        .ok_or_else(|| PError::expected("qx string delimiter"))?;
    if delim.is_alphanumeric() || delim.is_whitespace() {
        return Err(PError::expected("qx string delimiter"));
    }

    let (rest, content) = if let Some(close_char) = unicode_bracket_close(delim) {
        read_bracketed(after_qx, delim, close_char, true)?
    } else {
        let body = &after_qx[delim.len_utf8()..];
        let end = body
            .find(delim)
            .ok_or_else(|| PError::expected(&format!("closing '{delim}'")))?;
        (&body[end + delim.len_utf8()..], &body[..end])
    };

    let command_expr = if is_qq {
        interpolate_string_content(content)
    } else {
        // qx uses q-style backslash (only \\ → \)
        let s = content.replace("\\\\", "\\");
        Expr::Literal(Value::str(s))
    };

    Ok((
        rest,
        Expr::Call {
            name: Symbol::intern("QX"),
            args: vec![command_expr],
        },
    ))
}

/// Parse backtick command form: `...`.
/// This is equivalent to `qx` with interpolation enabled.
pub(crate) fn backtick_qx_string(input: &str) -> PResult<'_, Expr> {
    if crate::parser::stmt::simple::match_user_declared_circumfix_op(input).is_some() {
        return Err(PError::expected("backtick qx string"));
    }
    let body = input
        .strip_prefix('`')
        .ok_or_else(|| PError::expected("backtick qx string"))?;
    let end = body
        .find('`')
        .ok_or_else(|| PError::expected("closing '`'"))?;
    let content = &body[..end];
    let rest = &body[end + 1..];
    let command_expr = interpolate_string_content(content);
    Ok((
        rest,
        Expr::Call {
            name: Symbol::intern("QX"),
            args: vec![command_expr],
        },
    ))
}
