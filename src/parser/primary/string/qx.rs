use super::*;
use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

use super::helpers::literal_str;
/// Parse qx{...}, qqx{...} forms.
/// qx executes with single-quote (backslash) interpolation.
/// qqx executes with full interpolation.
pub(crate) fn qx_string(input: &str) -> PResult<'_, Expr> {
    // A declared `qx`/`qqx` wins over the quote language (see
    // `crate::parser::quote_shadow`).
    if crate::parser::quote_shadow::quote_lang_shadowed(input) {
        return Err(PError::expected("qx string"));
    }
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
    // `(` directly after `qx`/`qqx` is call syntax, never a delimiter.
    if delim.is_alphanumeric() || delim.is_whitespace() || delim == '(' {
        return Err(PError::expected("qx string delimiter"));
    }

    let (rest, content) = if let Some(close_char) = unicode_bracket_close(delim) {
        read_bracketed(after_qx, delim, close_char, true)?
    } else {
        let body = &after_qx[delim.len_utf8()..];
        let end = body
            .find(delim)
            .ok_or_else(|| PError::expected(format!("closing '{delim}'")))?;
        (&body[end + delim.len_utf8()..], &body[..end])
    };

    let command_expr = if is_qq {
        // qqx does full qq interpolation, including `{ EXPR }` closure blocks.
        // Closures are disabled only when the quote delimiter is itself a brace
        // (`qqx{ ... }`), where `{`/`}` are the delimiters, not interpolation.
        let closures = unicode_bracket_close(delim) != Some('}');
        interpolate_string_content_with_modes(content, true, closures)?
    } else {
        // qx uses q-style backslash (only \\ → \)
        let s = content.replace("\\\\", "\\");
        Expr::Literal(literal_str(s))
    };

    Ok((
        rest,
        Expr::Call {
            name: Symbol::intern("QX"),
            args: vec![command_expr],
            listop: false,
        },
    ))
}
