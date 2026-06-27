use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::split_angle_words;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::primary::string::{double_quoted_string, single_quoted_string};
use crate::symbol::Symbol;
use crate::value::Value;

/// Parse hash literal body: key => val, :name(val), ... }
pub(crate) fn parse_hash_literal_body(input: &str) -> PResult<'_, Expr> {
    let mut pairs = Vec::new();
    let mut spread_args = Vec::new();
    let mut rest = input;
    let mut pending_key: Option<String> = None;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            if pending_key.is_some() {
                return Err(PError::expected("hash value"));
            }
            if spread_args.is_empty() {
                return Ok((rest, Expr::Hash(pairs)));
            }
            let mut args = hash_args_from_pairs(pairs);
            args.append(&mut spread_args);
            return Ok((
                rest,
                Expr::Call {
                    name: Symbol::intern("hash"),
                    args,
                },
            ));
        }

        // Try colon pair syntax: :name(expr), :name, :!name, :$var, etc.
        if r.starts_with(':') && !r.starts_with("::") {
            if pending_key.is_some() {
                return Err(PError::expected("hash value"));
            }
            let (r_after_pair, (key, mut val)) = parse_colon_pair_entry(r)?;
            let (r_after_pair, _) = ws_inner(r_after_pair);
            let r_after_value = if let Some(after_arrow) = r_after_pair.strip_prefix("=>") {
                let (after_arrow, _) = ws_inner(after_arrow);
                let (after_arrow, explicit_val) = expression(after_arrow)?;
                val = Some(explicit_val);
                after_arrow
            } else {
                r_after_pair
            };
            pairs.push((key, val));
            let (r_after_value, _) = ws_inner(r_after_value);
            if let Some(stripped) = r_after_value.strip_prefix(',') {
                rest = stripped;
            } else if let Some(stripped) = r_after_value.strip_prefix(';') {
                rest = stripped;
            } else {
                rest = r_after_value;
            }
            continue;
        }

        if let Some(key) = pending_key.take() {
            let (r, val) = expression(r)?;
            pairs.push((key, Some(val)));
            let (r, _) = ws_inner(r);
            if let Some(stripped) = r.strip_prefix(',') {
                rest = stripped;
            } else if let Some(stripped) = r.strip_prefix(';') {
                rest = stripped;
            } else {
                rest = r;
            }
            continue;
        }

        if let Ok((r_key, key)) = parse_simple_hash_key(r) {
            let (after_key, _) = ws_inner(r_key);
            if let Some(after_arrow) = after_key.strip_prefix("=>") {
                let (after_arrow, _) = ws_inner(after_arrow);
                let (r_val, val) = expression(after_arrow)?;
                pairs.push((key, Some(val)));
                let (r_val, _) = ws_inner(r_val);
                if let Some(stripped) = r_val.strip_prefix(',') {
                    rest = stripped;
                } else if let Some(stripped) = r_val.strip_prefix(';') {
                    rest = stripped;
                } else {
                    rest = r_val;
                }
                continue;
            }

            // If followed by R=> (reverse fat arrow meta-operator), don't treat as
            // simple key — fall through to expression parsing which handles R=> correctly.
            if !after_key.starts_with("R=>") {
                pending_key = Some(key);
                let (r_key, _) = ws_inner(r_key);
                if let Some(stripped) = r_key.strip_prefix(',') {
                    rest = stripped;
                } else if let Some(stripped) = r_key.strip_prefix(';') {
                    rest = stripped;
                } else {
                    rest = r_key;
                }
                continue;
            }
        }

        let (r, item) = expression(r)?;
        match item {
            Expr::HashVar(name) => {
                spread_args.push(Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    expr: Box::new(Expr::HashVar(name)),
                });
            }
            Expr::Unary {
                op: crate::token_kind::TokenKind::Pipe,
                expr,
            } => {
                spread_args.push(Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    expr,
                });
            }
            other => {
                if let Some((key, val)) = hash_pair_from_expr(&other)? {
                    pairs.push((key, Some(val)));
                } else if let Expr::ArrayLiteral(elems) = &other {
                    // Flatten word lists like <a b c d> into pairs: a => "b", c => "d"
                    let mut i = 0;
                    while i + 1 < elems.len() {
                        let key = hash_key_from_expr(elems[i].clone())?;
                        pairs.push((key, Some(elems[i + 1].clone())));
                        i += 2;
                    }
                    if i < elems.len() {
                        // Odd element becomes a pending key
                        pending_key = Some(hash_key_from_expr(elems[i].clone())?);
                    }
                } else if matches!(other, Expr::Literal(_) | Expr::BareWord(_)) {
                    pending_key = Some(hash_key_from_expr(other)?);
                } else {
                    // A complex expression that is neither a `=>` pair nor a
                    // simple key (e.g. a ternary `$x ?? :$x !! ()` used as a
                    // hash element) is a BARE element: raku flattens it into the
                    // initializer (warning about odd counts only at runtime).
                    // Route it through the hash() builder via spread_args instead
                    // of failing with "expected hash key".
                    spread_args.push(other);
                }
            }
        }

        let (r, _) = ws_inner(r);
        if let Some(stripped) = r.strip_prefix(',') {
            rest = stripped;
        } else if let Some(stripped) = r.strip_prefix(';') {
            rest = stripped;
        } else {
            rest = r;
        }
    }
}

fn hash_args_from_pairs(pairs: Vec<(String, Option<Expr>)>) -> Vec<Expr> {
    pairs
        .into_iter()
        .map(|(key, val_opt)| Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(key))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(val_opt.unwrap_or(Expr::Literal(Value::Nil))),
        })
        .collect()
}

fn parse_simple_hash_key(input: &str) -> PResult<'_, String> {
    if let Ok((r, name)) = crate::parser::stmt::ident_pub(input) {
        return Ok((r, name));
    }
    if let Ok((r, Expr::Literal(Value::Int(n)))) = crate::parser::primary::number::integer(input) {
        return Ok((r, n.to_string()));
    }
    if let Ok((r, Expr::Literal(Value::BigInt(n)))) = crate::parser::primary::number::integer(input)
    {
        return Ok((r, n.to_string()));
    }
    if let Ok((r, Expr::Literal(Value::Str(s)))) =
        single_quoted_string(input).or_else(|_| double_quoted_string(input))
    {
        return Ok((r, s.to_string()));
    }
    Err(PError::expected("hash key"))
}

fn hash_key_from_expr(expr: Expr) -> Result<String, PError> {
    match expr {
        // Hash storage currently uses string keys; accept literal keys broadly and
        // normalize using Raku stringification semantics.
        Expr::Literal(v) => Ok(v.to_string_value()),
        Expr::BareWord(name) => Ok(name),
        _ => Err(PError::expected("hash key")),
    }
}

fn hash_pair_from_expr(expr: &Expr) -> Result<Option<(String, Expr)>, PError> {
    match expr {
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } => Ok(Some((
            hash_key_from_expr((**left).clone())?,
            (**right).clone(),
        ))),
        // R=> (reverse fat arrow): `1 R=> "a"` → key="a", value=1
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if meta == "R" && op == "=>" => Ok(Some((
            hash_key_from_expr((**right).clone())?,
            (**left).clone(),
        ))),
        Expr::PositionalPair(inner) => hash_pair_from_expr(inner),
        _ => Ok(None),
    }
}

/// Parse a colon pair entry inside a hash literal: :name(expr), :name, :!name, :Nname
fn parse_colon_pair_entry(input: &str) -> PResult<'_, (String, Option<Expr>)> {
    let r = input
        .strip_prefix(':')
        .ok_or_else(|| PError::expected("':'"))?;
    let digit_end = r
        .char_indices()
        .take_while(|(_, c)| crate::builtins::unicode::unicode_decimal_digit_value(*c).is_some())
        .last()
        .map(|(idx, c)| idx + c.len_utf8())
        .unwrap_or(0);
    if digit_end > 0 && r[digit_end..].starts_with('<') {
        return Err(PError::expected("generic radix literal"));
    }

    // :!name
    if let Some(r) = r.strip_prefix('!') {
        let (r, name) = crate::parser::stmt::ident_pub(r)?;
        return Ok((r, (name, Some(Expr::Literal(Value::Bool(false))))));
    }

    // :Nname — numeric colon pair, e.g., :1status means status => 1
    if let Some(first) = r.chars().next()
        && first.is_ascii_digit()
    {
        let digit_end = r.find(|c: char| !c.is_ascii_digit()).unwrap_or(r.len());
        let digits = &r[..digit_end];
        let after_digits = &r[digit_end..];
        if let Ok((r, name)) = crate::parser::stmt::ident_pub(after_digits) {
            let num: i64 = digits.parse().unwrap_or(0);
            return Ok((r, (name, Some(Expr::Literal(Value::Int(num))))));
        }
    }

    // :$var / :@var / :%var / :&var (with twigil support)
    if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&') {
        let sigil = &r[..1];
        let after_sigil = &r[1..];
        // Check for twigils: *, ?, ^, =, ~, :, !, .
        let (after_twigil, twigil) = if after_sigil.starts_with('*')
            || after_sigil.starts_with('?')
            || after_sigil.starts_with('^')
            || after_sigil.starts_with('=')
            || after_sigil.starts_with('~')
            || after_sigil.starts_with('!')
            || after_sigil.starts_with('.')
        {
            (&after_sigil[1..], &after_sigil[..1])
        } else if after_sigil.starts_with(':') && !after_sigil.starts_with("::") {
            (&after_sigil[1..], ":")
        } else {
            (after_sigil, "")
        };
        let (r, name) = crate::parser::stmt::ident_pub(after_twigil)?;
        let full_var_name = if twigil.is_empty() {
            name.clone()
        } else {
            format!("{}{}", twigil, name)
        };
        let expr = match sigil {
            "$" => Expr::Var(full_var_name),
            "@" => Expr::ArrayVar(full_var_name),
            "%" => Expr::HashVar(full_var_name),
            "&" => Expr::CodeVar(full_var_name),
            _ => unreachable!(),
        };
        return Ok((r, (name, Some(expr))));
    }

    // :name or :name(expr) or :name[items]
    let (r, name) = crate::parser::stmt::ident_pub(r)?;

    // :name(expr) or :name(expr, expr, ...)
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws_inner(r);
        let (r, first) = expression(r)?;
        let (r, _) = ws_inner(r);
        if r.starts_with(',') {
            let mut items = vec![first];
            let mut r = r;
            while r.starts_with(',') {
                let (r2, _) = parse_char(r, ',')?;
                let (r2, _) = ws_inner(r2);
                let (r2, next) = expression(r2)?;
                let (r2, _) = ws_inner(r2);
                items.push(next);
                r = r2;
            }
            let (r, _) = parse_char(r, ')')?;
            return Ok((r, (name, Some(Expr::ArrayLiteral(items)))));
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, (name, Some(first))));
    }

    // :name[items]
    if r.starts_with('[') {
        let (r, _) = parse_char(r, '[')?;
        let (r, _) = ws_inner(r);
        let mut items = Vec::new();
        let mut r = r;
        while !r.starts_with(']') {
            let (r2, item) = expression(r)?;
            items.push(item);
            let (r2, _) = ws_inner(r2);
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws_inner(r2);
                r = r2;
            } else {
                r = r2;
            }
        }
        let (r, _) = parse_char(r, ']')?;
        return Ok((r, (name, Some(Expr::BracketArray(items, false)))));
    }

    // :name{...} (block/hash-valued colonpair)
    if r.starts_with('{') {
        let (r, block_or_hash) = block_or_hash_expr(r)?;
        return Ok((r, (name, Some(block_or_hash))));
    }

    // :name<word> or :name<words> (angle bracket form)
    if r.starts_with('<') && !r.starts_with("<<") {
        let (r, _) = parse_char(r, '<')?;
        let end = r
            .find('>')
            .ok_or_else(|| PError::expected("'>' closing angle bracket"))?;
        let content = &r[..end];
        let r = &r[end + 1..];
        let words = split_angle_words(content);
        if words.len() == 1 {
            return Ok((
                r,
                (name, Some(Expr::Literal(Value::str(words[0].to_string())))),
            ));
        }
        let items = words
            .iter()
            .map(|w| Expr::Literal(Value::str(w.to_string())))
            .collect();
        return Ok((r, (name, Some(Expr::ArrayLiteral(items)))));
    }

    // :name«words» (French-quote form)
    if r.starts_with('\u{00AB}') {
        let (r, val_expr) = crate::parser::primary::container::french_quote_list(r)?;
        return Ok((r, (name, Some(val_expr))));
    }

    // :name<<words>> (double-angle form)
    if r.starts_with("<<") {
        let (r, val_expr) = crate::parser::primary::container::double_angle_list(r)?;
        return Ok((r, (name, Some(val_expr))));
    }

    // :name (boolean true)
    Ok((r, (name, None)))
}
