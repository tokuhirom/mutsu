use super::super::super::helpers::ws;
use super::super::super::parse_result::{PError, PResult, take_while1};
use super::super::ident;
use crate::ast::HandleSpec;

/// Parse a single handle spec item (colon-pair, word list, regex, wildcard, etc.)
fn parse_single_handle_spec<'a>(input: &'a str, specs: &mut Vec<HandleSpec>) -> PResult<'a, ()> {
    let r = input;
    // Colon-pair: :exposed<target> or :exposed('target')
    if let Some(after_colon) = r.strip_prefix(':') {
        let (after_name, name) = take_while1(after_colon, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        if let Some(after_angle) = after_name.strip_prefix('<') {
            // :name<target>
            let end = after_angle
                .find('>')
                .ok_or_else(|| PError::expected("closing > in handles pair"))?;
            let target = after_angle[..end].to_string();
            specs.push(HandleSpec::Rename {
                exposed: name.to_string(),
                target,
            });
            return Ok((&after_angle[end + 1..], ()));
        } else if let Some(after_paren) = after_name.strip_prefix('(') {
            // :name('target')
            let after_paren = after_paren.trim_start();
            if after_paren.starts_with('\'') || after_paren.starts_with('"') {
                let quote = after_paren.as_bytes()[0] as char;
                let after_quote = &after_paren[1..];
                let end = after_quote
                    .find(quote)
                    .ok_or_else(|| PError::expected("closing quote in handles pair"))?;
                let target = after_quote[..end].to_string();
                let after_close_quote = &after_quote[end + 1..];
                let after_close_quote = after_close_quote.trim_start();
                let after_close_paren = after_close_quote
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing ) in handles pair"))?;
                specs.push(HandleSpec::Rename {
                    exposed: name.to_string(),
                    target,
                });
                return Ok((after_close_paren, ()));
            }
        }
        return Err(PError::expected("handles pair value after colon-pair name"));
    }
    Err(PError::expected("handle spec"))
}

/// Parse handle specifications after the `handles` keyword.
pub(in crate::parser) fn parse_handle_specs<'a>(
    input: &'a str,
    specs: &mut Vec<HandleSpec>,
    rest_out: &mut &'a str,
) -> Result<(), PError> {
    let r = input;
    if let Some(r_inner) = r.strip_prefix('<') {
        // Word list: <a b c>
        let mut cursor = r_inner;
        loop {
            let (r_ws, _) = ws(cursor)?;
            cursor = r_ws;
            if let Some(r_end) = cursor.strip_prefix('>') {
                *rest_out = r_end;
                break;
            }
            let (r_name, method_name) = take_while1(cursor, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            specs.push(HandleSpec::Name(method_name.to_string()));
            cursor = r_name;
        }
    } else if let Some(after_star) = r.strip_prefix('*') {
        // Wildcard: *
        specs.push(HandleSpec::Wildcard);
        *rest_out = after_star;
    } else if let Some(after_slash) = r.strip_prefix('/') {
        // Regex: /pattern/
        let end = after_slash
            .find('/')
            .ok_or_else(|| PError::expected("closing / in handles regex"))?;
        let pattern = after_slash[..end].to_string();
        specs.push(HandleSpec::Regex(pattern));
        *rest_out = &after_slash[end + 1..];
    } else if let Some(after_paren) = r.strip_prefix('(') {
        // Parenthesized list: (:name<target>, :name2('target2'))
        let mut cursor = after_paren;
        loop {
            let (r_ws, _) = ws(cursor)?;
            cursor = r_ws;
            if let Some(r_end) = cursor.strip_prefix(')') {
                *rest_out = r_end;
                break;
            }
            // Skip commas
            if let Some(after_comma) = cursor.strip_prefix(',') {
                let (r_ws, _) = ws(after_comma)?;
                cursor = r_ws;
                continue;
            }
            let (after_spec, _) = parse_single_handle_spec(cursor, specs)?;
            cursor = after_spec;
        }
    } else if r.starts_with(':') {
        // Single colon-pair: :exposed<target>
        let (after_spec, _) = parse_single_handle_spec(r, specs)?;
        *rest_out = after_spec;
    } else if r.starts_with('\'') || r.starts_with('"') {
        // Quoted string: 'method' or "method"
        let quote = r.as_bytes()[0] as char;
        let after_open = &r[1..];
        let end = after_open
            .find(quote)
            .ok_or_else(|| PError::expected("closing quote in handles"))?;
        let method_name = &after_open[..end];
        specs.push(HandleSpec::Name(method_name.to_string()));
        *rest_out = &after_open[end + 1..];
    } else {
        // Bare identifier: could be a method name or type name
        let (r_name, method_name) = ident(r)?;
        specs.push(HandleSpec::Name(method_name.to_string()));
        *rest_out = r_name;
    }
    Ok(())
}
