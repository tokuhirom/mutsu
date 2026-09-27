//! `"&name(ARGS)"` interpolation: a code variable interpolates only as a call.
use crate::ast::Expr;
use crate::symbol::Symbol;

use super::helpers::literal_str;
use super::interp_helpers::try_parse_interp_method_call;
use super::interp_var::split_top_level_commas;

/// Interpolate `&name(ARGS)` at the head of `rest` (which starts at the `&`),
/// followed by the same postfix chain an interpolated variable takes
/// (`"&short-name($id).subst('::','-',:g)"` calls `.subst` on the result, as
/// in raku). Without the parenthesized argument list `&name` is literal text.
/// Arguments split at top-level commas only, so a comma inside a nested call
/// or a quoted string stays in its argument.
pub(super) fn try_code_call_interp<'a, F>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
    parse_postcircumfix_index: F,
) -> Option<&'a str>
where
    F: Fn(&'a str, Expr) -> (Expr, &'a str),
{
    let var_rest = rest.strip_prefix('&')?;
    let end = var_rest
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(var_rest.len());
    let name = &var_rest[..end];
    let after_name = &var_rest[end..];
    if !after_name.starts_with('(') {
        return None;
    }
    let mut depth = 0usize;
    let mut paren_end = None;
    for (idx, ch) in after_name.char_indices() {
        if ch == '(' {
            depth += 1;
        } else if ch == ')' {
            depth -= 1;
            if depth == 0 {
                paren_end = Some(idx);
                break;
            }
        }
    }
    let pe = paren_end?;
    let args_str = &after_name[1..pe];
    let args = if args_str.trim().is_empty() {
        vec![]
    } else {
        split_top_level_commas(args_str)
            .into_iter()
            .filter_map(|arg| {
                crate::parser::expr::expression(arg.trim())
                    .ok()
                    .map(|(_, expr)| expr)
            })
            .collect()
    };
    let call = Expr::Call {
        name: Symbol::intern(name),
        args,
    };
    let (expr, remainder) = parse_postcircumfix_index(&after_name[pe + 1..], call);
    let (expr, remainder) = try_parse_interp_method_call(remainder, expr);
    if !current.is_empty() {
        parts.push(Expr::Literal(literal_str(std::mem::take(current))));
    }
    parts.push(expr);
    Some(remainder)
}
