use super::*;
use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::{split_angle_words, ws};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::primary::parse_call_arg_list;
use crate::parser::primary::var::parse_ident_with_hyphens;
use crate::parser::stmt::keyword;
use crate::symbol::Symbol;
use crate::value::Value;
use crate::value::signature::{make_signature_value, param_defs_to_sig_info};
use std::collections::HashMap;

/// Parse colonpair expressions: :$var, :@var, :%var, :name(expr), :name, :!name
pub(crate) fn colonpair_expr(input: &str) -> PResult<'_, Expr> {
    let r = input
        .strip_prefix(':')
        .filter(|r| !r.starts_with(':'))
        .ok_or_else(|| PError::expected("colonpair"))?;
    // :36<...> is a generic radix literal, not a colonpair.
    let digit_end = r
        .char_indices()
        .take_while(|(_, c)| crate::builtins::unicode::unicode_decimal_digit_value(*c).is_some())
        .last()
        .map(|(idx, c)| idx + c.len_utf8())
        .unwrap_or(0);
    if digit_end > 0 && r[digit_end..].starts_with('<') {
        return Err(PError::expected("generic radix literal"));
    }
    // :2 or :36 alone (digits with no <, (, or [ suffix) is a malformed radix literal.
    // Only trigger this when the remaining part doesn't start with an identifier char
    // (to avoid blocking :123name colonpairs).
    if digit_end > 0 {
        let after = &r[digit_end..];
        let next_ch = after.chars().next();
        let is_ident_follow =
            next_ch.is_some_and(|c| c.is_alphabetic() || c == '_' || c == '-' || c == '\'');
        if !is_ident_follow
            && !after.starts_with('<')
            && !after.starts_with('(')
            && !after.starts_with('[')
        {
            return Err(PError::expected("generic radix literal"));
        }
    }
    // :{ ... }: typed hash literal (Hash[Mu,Any]).
    // Unlike bare { ... }, the colon prefix explicitly marks this as a hash,
    // so we always parse as a hash literal (never as a block).
    // Object hashes allow non-string keys (e.g., regex keys), so we use
    // expression-based parsing rather than the simple hash key heuristic.
    if let Some(after_brace) = r.strip_prefix('{') {
        let (inner, _) = ws_inner(after_brace);
        // Empty object hash: :{}
        if let Some(rest) = inner.strip_prefix('}') {
            return Ok((rest, Expr::Hash(Vec::new())));
        }
        return parse_object_hash_body(inner);
    }
    // :(...): signature literal.
    if let Some(mut r) = r.strip_prefix('(') {
        let (r2, _) = ws(r)?;
        // Preferred path: parse using the full parameter list parser.
        if let Ok((r3, (param_defs, return_type))) =
            crate::parser::stmt::parse_param_list_with_return_pub(r2)
            && let Ok((r3, _)) = ws(r3)
            && let Ok((r3, _)) = parse_char(r3, ')')
        {
            let sig_info = param_defs_to_sig_info(&param_defs, return_type);
            return Ok((r3, Expr::Literal(make_signature_value(sig_info))));
        }

        // Fallback path: permissive parsing for legacy forms like :(:$a = True)
        // used in roast tests around Test::Assuming.
        r = r2;
        let mut items = Vec::new();
        if r.starts_with(')') {
            let (r, _) = parse_char(r, ')')?;
            let mut attrs = HashMap::new();
            attrs.insert("raku".to_string(), Value::str_from(":()"));
            attrs.insert("perl".to_string(), Value::str_from(":()"));
            attrs.insert("Str".to_string(), Value::str_from(":()"));
            attrs.insert("gist".to_string(), Value::str_from(":()"));
            return Ok((
                r,
                Expr::Literal(Value::make_instance(Symbol::intern("Signature"), attrs)),
            ));
        }
        loop {
            let mut item_input = r;
            if let Ok((after_type, _type_name)) = parse_ident_with_hyphens(r) {
                let (after_ws, _) = ws(after_type)?;
                if after_ws.starts_with(':') {
                    item_input = after_ws;
                }
            }
            let (r_item, mut item) = match colonpair_expr(item_input) {
                Ok(parsed) => parsed,
                Err(_) => match expression(item_input) {
                    Ok((r_expr, expr_item)) => {
                        let (r_chk, _) = ws(r_expr)?;
                        let valid_tail = r_chk.starts_with(',')
                            || r_chk.starts_with(')')
                            || keyword("is", r_chk).is_some()
                            || keyword("where", r_chk).is_some()
                            || r_chk.starts_with('=');
                        if valid_tail {
                            (r_expr, expr_item)
                        } else {
                            let (r_frag, frag) = parse_signature_fragment(item_input)?;
                            (r_frag, Expr::BareWord(frag))
                        }
                    }
                    Err(_) => {
                        let (r_frag, frag) = parse_signature_fragment(item_input)?;
                        (r_frag, Expr::BareWord(frag))
                    }
                },
            };
            let (mut r_next, _) = ws(r_item)?;
            while let Some(after_is) = keyword("is", r_next) {
                let (after_is, _) = ws(after_is)?;
                let (after_is, _trait_name) = parse_ident_with_hyphens(after_is)?;
                let (after_is, _) = ws(after_is)?;
                r_next = after_is;
            }
            if let Some(after_where) = keyword("where", r_next) {
                let (after_where, _) = ws(after_where)?;
                let (after_where, _constraint) = expression(after_where)?;
                let (after_where, _) = ws(after_where)?;
                r_next = after_where;
            }
            if let Some(after_eq) = r_next.strip_prefix('=')
                && let Expr::Binary { left, op, .. } = &item
                && *op == crate::token_kind::TokenKind::FatArrow
            {
                let (after_eq, _) = ws(after_eq)?;
                let (after_eq, value_expr) = expression(after_eq)?;
                item = Expr::Binary {
                    left: left.clone(),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(value_expr),
                };
                let (after_eq, _) = ws(after_eq)?;
                r_next = after_eq;
            }
            items.push(item);
            if r_next.starts_with(',') {
                let (r_more, _) = parse_char(r_next, ',')?;
                let (r_more, _) = ws(r_more)?;
                r = r_more;
                continue;
            }
            let (r_end, _) = parse_char(r_next, ')')?;
            let rendered = items
                .iter()
                .map(render_signature_item)
                .collect::<Vec<_>>()
                .join(", ");
            let sig = format!(":({})", rendered);
            let mut attrs = HashMap::new();
            attrs.insert("raku".to_string(), Value::str(sig.clone()));
            attrs.insert("perl".to_string(), Value::str(sig.clone()));
            attrs.insert("Str".to_string(), Value::str(sig.clone()));
            attrs.insert("gist".to_string(), Value::str(sig));
            return Ok((
                r_end,
                Expr::Literal(Value::make_instance(Symbol::intern("Signature"), attrs)),
            ));
        }
    }
    // :16(expr) — radix conversion shorthand (equivalent to UNBASE(16, expr))
    {
        let mut digit_end = 0;
        let mut base_clean = String::new();
        for c in r.chars() {
            let Some(dv) = crate::builtins::unicode::unicode_decimal_digit_value(c) else {
                break;
            };
            digit_end += c.len_utf8();
            base_clean.push(char::from_digit(dv, 10).unwrap());
        }
        if digit_end > 0 {
            let after_digits = &r[digit_end..];
            if let Some(after_paren) = after_digits.strip_prefix('(') {
                let base: u32 = base_clean.parse().unwrap_or(0);
                if !(2..=36).contains(&base) {
                    return Err(PError::expected("generic radix base 2..36"));
                }
                let (r_args, _) = ws(after_paren)?;
                let (r_args, mut args) = parse_call_arg_list(r_args)?;
                let (r_args, _) = ws(r_args)?;
                let (r_args, _) = parse_char(r_args, ')')?;
                let mut call_args = vec![Expr::Literal(Value::Int(base as i64))];
                call_args.append(&mut args);
                return Ok((
                    r_args,
                    Expr::Call {
                        name: Symbol::intern("UNBASE"),
                        args: call_args,
                    },
                ));
            }
            // :N[list] — radix list notation (equivalent to RADIX_LIST(N, list...))
            // The list form supports any base >= 2 (no 36 limit since digits are numeric values)
            if let Some(after_bracket) = after_digits.strip_prefix('[') {
                let base: u64 = base_clean.parse().unwrap_or(0);
                if base >= 2 {
                    let (r_items, _) = ws(after_bracket)?;
                    let mut items = Vec::new();
                    let mut r_items = r_items;
                    while !r_items.starts_with(']') {
                        let (r2, item) = expression(r_items)?;
                        items.push(item);
                        let (r2, _) = ws(r2)?;
                        if r2.starts_with(',') {
                            let (r2, _) = parse_char(r2, ',')?;
                            let (r2, _) = ws(r2)?;
                            r_items = r2;
                        } else {
                            r_items = r2;
                        }
                    }
                    let (r_items, _) = parse_char(r_items, ']')?;
                    let base_expr = if let Ok(b) = i64::try_from(base) {
                        Expr::Literal(Value::Int(b))
                    } else {
                        Expr::Literal(Value::bigint(num_bigint::BigInt::from(base)))
                    };
                    let mut call_args = vec![base_expr];
                    call_args.extend(items);
                    return Ok((
                        r_items,
                        Expr::Call {
                            name: Symbol::intern("RADIX_LIST"),
                            args: call_args,
                        },
                    ));
                }
            }
        }
    }
    // :123name (numeric leading-value pair) => :name(123)
    // Handles ASCII digits and Unicode Nd (decimal digit) characters
    {
        let mut digit_end = 0;
        let mut numeric_value: Option<i64> = Some(0);
        let mut use_bigint = false;
        for c in r.chars() {
            let dv = if c.is_ascii_digit() {
                Some(c as u32 - '0' as u32)
            } else {
                crate::builtins::unicode::unicode_decimal_digit_value(c)
            };
            if let Some(d) = dv {
                if let Some(ref mut v) = numeric_value {
                    match v.checked_mul(10).and_then(|v2| v2.checked_add(d as i64)) {
                        Some(new_v) => *v = new_v,
                        None => use_bigint = true,
                    }
                }
                digit_end += c.len_utf8();
            } else {
                break;
            }
        }
        if digit_end > 0 && digit_end < r.len() {
            let digits_str = &r[..digit_end];
            let after_digits = &r[digit_end..];
            if let Ok((rest, name)) = parse_ident_with_hyphens(after_digits) {
                let val = if use_bigint {
                    // Parse as BigInt for numbers that overflow i64
                    // Collect decimal digit values and build the number string
                    let digit_string: String = digits_str
                        .chars()
                        .filter_map(|c| {
                            let dv = if c.is_ascii_digit() {
                                Some(c as u32 - '0' as u32)
                            } else {
                                crate::builtins::unicode::unicode_decimal_digit_value(c)
                            };
                            dv.map(|d| char::from_digit(d, 10).unwrap())
                        })
                        .collect();
                    if let Ok(n) = digit_string.parse::<num_bigint::BigInt>() {
                        Value::bigint(n)
                    } else {
                        Value::Int(0)
                    }
                } else if numeric_value.is_some() {
                    Value::Int(numeric_value.unwrap())
                } else {
                    Value::Int(0)
                };
                // Numeric adverb can't have an extra value: :69th($_) is an error
                if rest.starts_with('(') {
                    return Err(PError::expected(
                        "no extra argument for numeric colonpair (pair already has a numeric value)",
                    ));
                }
                return Ok((
                    rest,
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                        op: crate::token_kind::TokenKind::FatArrow,
                        right: Box::new(Expr::Literal(val)),
                    },
                ));
            }
        }
    }
    // :!name (negated boolean pair)
    if let Some(after_bang) = r.strip_prefix('!') {
        let (rest, name) = parse_ident_with_hyphens(after_bang)?;
        // A negated pair may not carry an argument: `:!foo(3)` is
        // X::Syntax::NegatedPair ("Argument not allowed on negated pair").
        if rest.starts_with('(') {
            let message = format!("Argument not allowed on negated pair with key '{}'", name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("key".to_string(), Value::str(name.to_string()));
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::NegatedPair"), attrs);
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::Literal(Value::Bool(false))),
            },
        ));
    }
    // :$var / :@var / :%var / :&var (autopair from variable, with twigil support)
    if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') || r.starts_with('&') {
        let sigil = &r[..1];
        let after_sigil = &r[1..];
        // Handle $<name> / @<name> / %<name> capture variable twigils
        if (sigil == "$" || sigil == "@" || sigil == "%")
            && after_sigil.starts_with('<')
            && let Some(end) = after_sigil[1..].find('>')
        {
            let name = &after_sigil[1..1 + end];
            let rest = &after_sigil[1 + end + 1..];
            let var_expr = Expr::CaptureVar(name.to_string());
            return Ok((
                rest,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(var_expr),
                },
            ));
        }
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
        let (mut rest, var_name) = parse_ident_with_hyphens(after_twigil)?;
        // Strip trailing ? or ! for adverb pair forms (:name? :name!)
        if twigil.is_empty() && (rest.starts_with('?') || rest.starts_with('!')) {
            rest = &rest[1..];
        }
        let key = var_name.to_string();
        let full_var_name = if twigil.is_empty() {
            var_name.to_string()
        } else {
            format!("{}{}", twigil, var_name)
        };
        let var_expr = match sigil {
            "$" => Expr::Var(full_var_name),
            "@" => Expr::ArrayVar(full_var_name),
            "%" => Expr::HashVar(full_var_name),
            "&" => Expr::CodeVar(full_var_name),
            _ => unreachable!(),
        };
        return Ok((
            rest,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(key))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(var_expr),
            },
        ));
    }
    // :name(expr) or :name (boolean true pair)
    let (mut rest, name) = parse_ident_with_hyphens(r)?;
    if rest.starts_with('?') || rest.starts_with('!') {
        rest = &rest[1..];
    }
    // Don't parse statement-modifier keywords as bare colonpairs (`:when`),
    // but allow them when followed by a value (`:when<now>`, `:if(True)`, etc.)
    if matches!(
        name,
        "if" | "unless" | "for" | "while" | "until" | "given" | "when"
    ) && !rest.starts_with('(')
        && !rest.starts_with('[')
        && !rest.starts_with('{')
        && !rest.starts_with('<')
        && !rest.starts_with('\u{00AB}')
    {
        return Err(PError::expected("colonpair name"));
    }
    // Consume unspace between colonpair name and value: :foo\ ("bar")
    let rest = crate::parser::helpers::consume_unspace(rest);
    if let Some(after_paren) = rest.strip_prefix('(') {
        let (r, _) = ws(after_paren)?;
        // Handle empty parens: :name() → name => ()
        if let Some(r) = r.strip_prefix(')') {
            return Ok((
                r,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::ArrayLiteral(Vec::new())),
                },
            ));
        }
        let (r, first) = expression(r)?;
        let (r, _) = ws(r)?;
        // Check for separated list: :name(a, b, ...) or :name(a; b; ...) or :name(a,)
        if r.starts_with(',') || (r.starts_with(';') && !r.starts_with(";;")) {
            let mut items = vec![first];
            let mut r = r;
            while r.starts_with(',') || (r.starts_with(';') && !r.starts_with(";;")) {
                let sep = if r.starts_with(',') { ',' } else { ';' };
                let (r2, _) = parse_char(r, sep)?;
                let (r2, _) = ws(r2)?;
                // Trailing comma: :name(a,)
                if r2.starts_with(')') {
                    r = r2;
                    break;
                }
                let (r2, next) = expression(r2)?;
                let (r2, _) = ws(r2)?;
                items.push(next);
                r = r2;
            }
            let (r, _) = parse_char(r, ')')?;
            return Ok((
                r,
                Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                    op: crate::token_kind::TokenKind::FatArrow,
                    right: Box::new(Expr::ArrayLiteral(items)),
                },
            ));
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(first),
            },
        ));
    }
    // :name[items] (array-valued colonpair)
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let mut items = Vec::new();
        let mut r = r;
        while !r.starts_with(']') {
            let (r2, item) = expression(r)?;
            items.push(item);
            let (r2, _) = ws(r2)?;
            if r2.starts_with(',') {
                let (r2, _) = parse_char(r2, ',')?;
                let (r2, _) = ws(r2)?;
                r = r2;
            } else {
                r = r2;
            }
        }
        let (r, _) = parse_char(r, ']')?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(Expr::BracketArray(items, false)),
            },
        ));
    }
    // :name{ ... } (block/hash-valued colonpair)
    if rest.starts_with('{') {
        let (r, block_or_hash) = block_or_hash_expr(rest)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(block_or_hash),
            },
        ));
    }
    // :name<value> (angle-bracket colonpair, equivalent to :name("value"))
    if rest.starts_with('<') && !rest.starts_with("<<") && !rest.starts_with("<=") {
        let inner = &rest[1..];
        if let Some(close) = crate::parser::primary::container::find_nested_angle_close_pub(inner) {
            let content = &inner[..close];
            let r = &inner[close + 1..];
            let words = split_angle_words(content);
            if !words.is_empty() {
                let val_expr = if words.len() == 1 {
                    Expr::Literal(crate::parser::primary::container::angle_word_value(
                        words[0],
                    ))
                } else {
                    Expr::ArrayLiteral(
                        words
                            .into_iter()
                            .map(|w| {
                                Expr::Literal(crate::parser::primary::container::angle_word_value(
                                    w,
                                ))
                            })
                            .collect(),
                    )
                };
                return Ok((
                    r,
                    Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                        op: crate::token_kind::TokenKind::FatArrow,
                        right: Box::new(val_expr),
                    },
                ));
            }
        }
    }
    // :name«words» (French-quote colonpair, equivalent to :name(«words»))
    if rest.starts_with('\u{00AB}') {
        let (r, val_expr) = crate::parser::primary::container::french_quote_list(rest)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(val_expr),
            },
        ));
    }
    // :name<<words>> (double-angle colonpair, equivalent to :name(<<words>>))
    if rest.starts_with("<<") {
        let (r, val_expr) = crate::parser::primary::container::double_angle_list(rest)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::str(name.to_string()))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(val_expr),
            },
        ));
    }
    // :name (boolean true)
    Ok((
        rest,
        Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(name.to_string()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Bool(true))),
        },
    ))
}

fn render_signature_item(expr: &Expr) -> String {
    match expr {
        Expr::BareWord(name) => name.clone(),
        Expr::Var(name) => format!("${}", name),
        Expr::ArrayVar(name) => format!("@{}", name),
        Expr::HashVar(name) => format!("%{}", name),
        Expr::Binary { left, op, right } if *op == crate::token_kind::TokenKind::FatArrow => {
            if let Expr::Literal(Value::Str(name)) = left.as_ref() {
                match right.as_ref() {
                    Expr::Var(v) if v.as_str() == name.as_str() => format!(":${}", name),
                    Expr::Literal(Value::Bool(true)) => format!(":${}", name),
                    other => format!(":${} = {}", name, render_signature_item(other)),
                }
            } else {
                "...".to_string()
            }
        }
        Expr::Literal(v) => match v {
            Value::Str(s) => format!("\"{}\"", s.replace('"', "\\\"")),
            _ => v.to_string_value(),
        },
        Expr::AnonSub { .. } => "{ ... }".to_string(),
        _ => "...".to_string(),
    }
}

fn parse_signature_fragment(input: &str) -> PResult<'_, String> {
    let mut depth_paren = 0usize;
    let mut depth_bracket = 0usize;
    let mut depth_brace = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escape = false;
    for (idx, ch) in input.char_indices() {
        if in_single {
            if ch == '\'' && !escape {
                in_single = false;
            }
            escape = ch == '\\' && !escape;
            continue;
        }
        if in_double {
            if ch == '"' && !escape {
                in_double = false;
            }
            escape = ch == '\\' && !escape;
            continue;
        }
        match ch {
            '\'' => in_single = true,
            '"' => in_double = true,
            '(' => depth_paren += 1,
            ')' => {
                if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 {
                    let frag = input[..idx].trim();
                    if frag.is_empty() {
                        return Err(PError::expected("signature item"));
                    }
                    return Ok((&input[idx..], frag.to_string()));
                }
                depth_paren = depth_paren.saturating_sub(1);
            }
            '[' => depth_bracket += 1,
            ']' => depth_bracket = depth_bracket.saturating_sub(1),
            '{' => depth_brace += 1,
            '}' => depth_brace = depth_brace.saturating_sub(1),
            ',' if depth_paren == 0 && depth_bracket == 0 && depth_brace == 0 => {
                let frag = input[..idx].trim();
                if frag.is_empty() {
                    return Err(PError::expected("signature item"));
                }
                return Ok((&input[idx..], frag.to_string()));
            }
            _ => {}
        }
    }
    let frag = input.trim();
    if frag.is_empty() {
        return Err(PError::expected("signature item"));
    }
    Ok(("", frag.to_string()))
}

/// Parse the body of an object hash `:{ ... }`.
/// Object hashes allow arbitrary expression keys (e.g., regex), so we parse
/// each entry using full expression parsing and emit a `hash(...)` call with
/// Pair arguments, preserving expression-typed keys at runtime.
fn parse_object_hash_body(input: &str) -> PResult<'_, Expr> {
    let mut args = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            return Ok((
                rest,
                Expr::Call {
                    name: crate::symbol::Symbol::intern("hash"),
                    args,
                },
            ));
        }

        // Parse each entry as a full expression (which handles `expr => expr` as
        // a fat arrow binary). This allows regex keys like `rx/.../ => "..."`.
        let (r, item) = expression(r)?;
        args.push(item);

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
