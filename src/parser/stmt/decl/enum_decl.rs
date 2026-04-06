use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};
use super::super::{ident, keyword, qualified_ident};
use super::take_while_opt;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Parse `anon enum` declaration.
pub(crate) fn anon_enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("anon", input).ok_or_else(|| PError::expected("anon enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("enum", rest).ok_or_else(|| PError::expected("enum after anon"))?;
    let (rest, _) = ws1(rest)?;
    parse_anon_enum_body(rest)
}

/// Parse `enum` declaration.
pub(crate) fn enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("enum", input).ok_or_else(|| PError::expected("enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    // Anonymous enum: `enum < foo bar >` or `enum :: < foo bar >`
    if rest.starts_with('<') || rest.starts_with('(') {
        return parse_anon_enum_body(rest);
    }
    if let Some(r) = rest.strip_prefix("::") {
        let (r, _) = ws(r)?;
        if r.starts_with('<') || r.starts_with('(') {
            return parse_anon_enum_body(r);
        }
    }
    parse_enum_decl_body(rest)
}

/// Parse anonymous enum body (after `enum` keyword with no name).
fn parse_anon_enum_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, variants) = if input.starts_with("<<") || input.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(input)?
    } else if input.starts_with('<') {
        let (r, _) = parse_char(input, '<')?;
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
    } else if input.starts_with('(') {
        let (r, _) = parse_char(input, '(')?;
        let (r, _) = ws(r)?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            if let Some(r2) = r.strip_prefix(')') {
                r = r2;
                break;
            }
            let (r2, variant) = parse_enum_variant_entry(r)?;
            variants.push(variant);
            let (r2, _) = ws(r2)?;
            if let Some(stripped) = r2.strip_prefix(',') {
                let (r2, _) = ws(stripped)?;
                r = r2;
            } else {
                r = r2;
            }
        }
        (r, variants)
    } else {
        return Err(PError::expected("anonymous enum variants"));
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::EnumDecl {
            name: Symbol::intern(""),
            variants,
            is_export: false,
            base_type: None,
            language_version: super::super::simple::current_language_version(),
        },
    ))
}

/// Parse `<< ... >>` or `\u{ab} ... \u{bb}` enum variant list.
/// Supports plain words and colonpairs like `:key<value>` or `:key[expr, ...]`.
fn parse_double_angle_enum_variants(input: &str) -> PResult<'_, Vec<(String, Option<Expr>)>> {
    let (r, use_unicode_close) = if let Some(r) = input.strip_prefix("<<") {
        (r, false)
    } else if let Some(r) = input.strip_prefix('\u{ab}') {
        // «
        (r, true)
    } else {
        return Err(PError::expected("<< or \u{ab}"));
    };
    let mut variants = Vec::new();
    let mut r = r;
    loop {
        // Skip whitespace (including newlines)
        let (r2, _) = ws(r)?;
        r = r2;
        // Check for closing >> or »
        if use_unicode_close {
            if let Some(r2) = r.strip_prefix('\u{bb}') {
                return Ok((r2, variants));
            }
        } else if let Some(r2) = r.strip_prefix(">>") {
            return Ok((r2, variants));
        }
        // Colonpair: :key<value> or :key[expr, ...] or :!key
        if r.starts_with(':') && !r.starts_with("::") {
            let after_colon = &r[1..];
            // Handle :!key (negated boolean)
            let (after_neg, negated) = if let Some(stripped) = after_colon.strip_prefix('!') {
                (stripped, true)
            } else {
                (after_colon, false)
            };
            // Parse the key (identifier)
            let (after_key, key) = take_while1(after_neg, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            if negated {
                // :!key => value is 0 (false)
                variants.push((key.to_string(), Some(Expr::Literal(Value::Int(0.into())))));
                r = after_key;
            } else if let Some(after_open) = after_key.strip_prefix('<') {
                // :key<value>
                let (after_val, val) = take_while1(after_open, |c: char| c != '>')?;
                let after_close = after_val
                    .strip_prefix('>')
                    .ok_or_else(|| PError::expected(">"))?;
                variants.push((key.to_string(), Some(Expr::Literal(Value::str_from(val)))));
                r = after_close;
            } else if after_key.starts_with('[') {
                // :key[expr, ...] — parse as array expression
                let (after_expr, expr) = expression(after_key)?;
                variants.push((key.to_string(), Some(expr)));
                r = after_expr;
            } else if after_key.starts_with('(') {
                // :key(expr) — parse as expression in parens
                let (after_expr, expr) = expression(after_key)?;
                variants.push((key.to_string(), Some(expr)));
                r = after_expr;
            } else {
                // :key with no value — treat as boolean true (no explicit value)
                variants.push((key.to_string(), None));
                r = after_key;
            }
        } else {
            // Plain word
            let close_char = if use_unicode_close { '\u{bb}' } else { '>' };
            let (r2, word) = take_while1(r, |c: char| {
                !c.is_whitespace() && c != close_char && c != '>' && c != ':'
            })?;
            variants.push((word.to_string(), None));
            r = r2;
        }
    }
}

/// Try to parse all enum variants as static entries inside `(...)`.
fn parse_static_enum_variants(input: &str) -> PResult<'_, Vec<(String, Option<Expr>)>> {
    let mut variants = Vec::new();
    let mut r = input;
    loop {
        let (r2, _) = ws(r)?;
        if let Some(r2) = r2.strip_prefix(')') {
            return Ok((r2, variants));
        }
        let (r2, variant) = parse_enum_variant_entry(r)?;
        variants.push(variant);
        let (r2, _) = ws(r2)?;
        if let Some(stripped) = r2.strip_prefix(',') {
            let (r2, _) = ws(stripped)?;
            r = r2;
        } else {
            r = r2;
        }
    }
}

fn parse_enum_variant_entry(input: &str) -> PResult<'_, (String, Option<Expr>)> {
    let (rest, expr) = expression(input)?;
    match expr {
        Expr::BareWord(name) => Ok((rest, (name, None))),
        Expr::Literal(Value::Str(name)) => Ok((rest, (name.to_string(), None))),
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } => match *left {
            Expr::Literal(Value::Str(name)) => {
                let value_expr = match *right {
                    Expr::Literal(Value::Bool(true)) => None,
                    other => Some(other),
                };
                Ok((rest, (name.to_string(), value_expr)))
            }
            _ => Err(PError::expected("enum variant name")),
        },
        // Handle PositionalPair wrapping a Pair (e.g., from `"foo" => -42`)
        Expr::PositionalPair(inner) => match *inner {
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } => match *left {
                Expr::Literal(Value::Str(name)) => {
                    let value_expr = match *right {
                        Expr::Literal(Value::Bool(true)) => None,
                        other => Some(other),
                    };
                    Ok((rest, (name.to_string(), value_expr)))
                }
                _ => Err(PError::expected("enum variant name")),
            },
            _ => Err(PError::expected("enum variant")),
        },
        _ => Err(PError::expected("enum variant")),
    }
}

pub(in crate::parser::stmt) fn parse_enum_decl_body(input: &str) -> PResult<'_, Stmt> {
    parse_enum_decl_body_with_type(input, None)
}

pub(super) fn parse_enum_decl_body_with_type(
    input: &str,
    base_type: Option<String>,
) -> PResult<'_, Stmt> {
    let (rest, name_str) = qualified_ident(input)?;
    let name = Symbol::intern(&name_str);
    let (rest, _) = ws(rest)?;

    // Parse `is <trait>` clauses (e.g., `is export`)
    let mut rest = rest;
    let mut is_export = false;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "export" {
            is_export = true;
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // Enum variants in << >>, « », <> or ()
    let (rest, variants) = if rest.starts_with("<<") || rest.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(rest)?
    } else if rest.starts_with('<') {
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
        // Try static variant parsing first; if any entry is not a simple
        // identifier/string/pair, fall back to treating the whole body as
        // a dynamic expression.
        if let Ok(static_result) = parse_static_enum_variants(r) {
            let (r, variants) = static_result;
            (r, variants)
        } else {
            // Dynamic: parse as expression list (may use operators like X~, Z=>, |, etc.)
            let (r2, expr) = expression(r)?;
            let (r2, _) = ws(r2)?;
            if let Some(r3) = r2.strip_prefix(',') {
                let mut items = vec![expr];
                let (mut r, _) = ws(r3)?;
                loop {
                    let (r2, _) = ws(r)?;
                    if let Some(r2) = r2.strip_prefix(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item_expr) = expression(r)?;
                    items.push(item_expr);
                    let (r2, _) = ws(r2)?;
                    if let Some(stripped) = r2.strip_prefix(',') {
                        let (r2, _) = ws(stripped)?;
                        r = r2;
                    } else {
                        r = r2;
                    }
                }
                let combined = Expr::ArrayLiteral(items);
                return Ok((
                    r,
                    Stmt::EnumDecl {
                        name,
                        variants: vec![("__DYNAMIC__".to_string(), Some(combined))],
                        is_export,
                        base_type: base_type.clone(),
                        language_version: super::super::simple::current_language_version(),
                    },
                ));
            }
            let r2 = r2
                .strip_prefix(')')
                .ok_or_else(|| PError::expected("closing ')' for enum body"))?;
            return Ok((
                r2,
                Stmt::EnumDecl {
                    name,
                    variants: vec![("__DYNAMIC__".to_string(), Some(expr))],
                    is_export,
                    base_type: base_type.clone(),
                    language_version: super::super::simple::current_language_version(),
                },
            ));
        }
    } else {
        (rest, Vec::new())
    };

    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::EnumDecl {
            name,
            variants,
            is_export,
            base_type,
            language_version: super::super::simple::current_language_version(),
        },
    ))
}
