//! `@.meth(args)` / `@.meth: args` (and the `%` spelling): a method call on
//! `self` whose result is put in the sigil's context.
//!
//! rakudo's `variable` token lets a `.`-twigil variable carry its own argument
//! list -- a `(...)` postcircumfix or a `:` followed by whitespace and an
//! arglist -- and then applies the sigil's contextualizer to the call's result:
//! `@.protect: { 42 }` is `@(self.protect({ 42 }))`, so it prints `(42)`. The
//! `$` spelling needs no wrapper and is left to the ordinary postfix parser
//! (see `scalar_var`).

use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::symbol::Symbol;

/// If `rest` (just past the method name of `@.name` / `%.name`) starts an
/// argument list, parse it and return the contextualized call. `context` is
/// the contextualizer method: `list` for `@`, `hash` for `%`. Returns `None`
/// when no argument list follows, leaving the bare accessor form to the caller.
pub(super) fn contextualized_self_call<'a>(
    rest: &'a str,
    name: &str,
    context: &str,
) -> Option<PResult<'a, Expr>> {
    let parsed = if rest.starts_with('(') {
        paren_args(rest)
    } else if let Some(after_colon) = rest.strip_prefix(':')
        && after_colon.starts_with(char::is_whitespace)
    {
        colon_args(rest)
    } else {
        return None;
    };
    Some(parsed.map(|(r, args)| {
        let call = Expr::MethodCall {
            target: Box::new(Expr::BareWord("self".to_string())),
            name: Symbol::intern(name),
            args,
            modifier: None,
            quoted: false,
            sugar: false,
        };
        (
            r,
            Expr::MethodCall {
                target: Box::new(call),
                name: Symbol::intern(context),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: false,
            },
        )
    }))
}

fn paren_args(input: &str) -> PResult<'_, Vec<Expr>> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, args) = crate::parser::primary::parse_call_arg_list(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, ')')?;
    Ok((r, args))
}

/// `input` starts at the `:`. An empty arglist (`@.meth: ;`) is a zero-argument
/// call, as for the postfix `.meth:` form.
fn colon_args(input: &str) -> PResult<'_, Vec<Expr>> {
    let (after_ws, _) = ws(&input[1..])?;
    if after_ws.is_empty() || after_ws.starts_with([';', '}', ')']) {
        return Ok((after_ws, Vec::new()));
    }
    crate::parser::stmt::assign::parse_colon_args(input)
}

/// `$.name:sym<x>(args)`: a `.`-twigil variable's name is a *longname*, so a
/// `:key<value>` colonpair glued to the identifier is part of the method name
/// (`$.numeric:sym<frac>($/)` in PDF::Grammar's actions calls the method
/// declared as `method numeric:sym<frac>`). `after_ident` is the input just
/// past the identifier. Returns `None` unless a `:ident<...>` colonpair
/// follows; then the call is `self."name:ident<...>"(args)`, with the
/// arguments taken only from a directly following `(...)`.
pub(super) fn longname_self_call<'a>(
    after_ident: &'a str,
    ident: &str,
) -> Option<PResult<'a, Expr>> {
    let after_colon = after_ident.strip_prefix(':')?;
    let key_len = after_colon
        .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-'))
        .unwrap_or(after_colon.len());
    if key_len == 0 || !after_colon.starts_with(|c: char| c.is_alphabetic() || c == '_') {
        return None;
    }
    let value_start = &after_colon[key_len..];
    let value_body = value_start.strip_prefix('<')?;
    let close = value_body.find('>')?;
    let value = &value_body[..close];
    if value.is_empty() || value.contains(['<', '\n']) {
        return None;
    }
    let name = format!("{ident}:{}<{value}>", &after_colon[..key_len]);
    let rest = &value_body[close + 1..];
    let (rest, args) = if rest.starts_with('(') {
        match paren_args(rest) {
            Ok(parsed) => parsed,
            Err(err) => return Some(Err(err)),
        }
    } else {
        (rest, Vec::new())
    };
    Some(Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(Expr::BareWord("self".to_string())),
            name: Symbol::intern(&name),
            args,
            modifier: None,
            quoted: false,
            sugar: false,
        },
    )))
}
