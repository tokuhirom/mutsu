use super::super::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};

use crate::ast::Expr;
use crate::value::Value;

use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws};

#[derive(Default)]
struct MatchAdverbs {
    exhaustive: bool,
    repeat: Option<usize>,
}

fn parse_match_adverbs(input: &str) -> PResult<'_, MatchAdverbs> {
    let mut spec = input;
    let mut adverbs = MatchAdverbs::default();
    loop {
        if !spec.starts_with(':') {
            break;
        }
        let mut r = &spec[1..];
        let mut leading_digits = String::new();
        while let Some(ch) = r.chars().next() {
            if ch.is_ascii_digit() {
                leading_digits.push(ch);
                r = &r[ch.len_utf8()..];
            } else {
                break;
            }
        }

        let mut name = String::new();
        while let Some(ch) = r.chars().next() {
            if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                name.push(ch);
                r = &r[ch.len_utf8()..];
            } else {
                break;
            }
        }
        if name.is_empty() {
            break;
        }

        let mut arg: Option<&str> = None;
        if r.starts_with('(') {
            let after = skip_balanced_parens(r);
            if after == r {
                return Err(PError::expected("closing ')' in regex modifier"));
            }
            arg = Some(&r[1..r.len() - after.len() - 1]);
            r = after;
        }

        if name == "ex" || name == "exhaustive" {
            adverbs.exhaustive = true;
        } else if name == "x" {
            if let Some(raw) = arg {
                let trimmed = raw.trim();
                if !trimmed.is_empty()
                    && let Ok(count) = trimmed.parse::<usize>()
                {
                    adverbs.repeat = Some(count);
                }
            } else if !leading_digits.is_empty()
                && let Ok(count) = leading_digits.parse::<usize>()
            {
                adverbs.repeat = Some(count);
            }
        } else if !leading_digits.is_empty()
            && name == "x"
            && let Ok(count) = leading_digits.parse::<usize>()
        {
            adverbs.repeat = Some(count);
        }

        spec = r;
    }
    Ok((spec, adverbs))
}

/// Parse comma-separated call arguments inside parens.
pub(in crate::parser) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
    if input.starts_with(')') {
        return Ok((input, Vec::new()));
    }
    let (input, first) = expression(input)?;
    let mut args = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            return Ok((r, args));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, args));
        }
        let (r, arg) = expression(r)?;
        args.push(arg);
        rest = r;
    }
}

/// Scan `input` for content delimited by `close_ch`, handling backslash escapes,
/// single-quoted strings, and paired-delimiter nesting.
/// Returns `(content, rest_after_close)` or None.
pub(in crate::parser) fn scan_to_delim(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    let mut depth = 1u32;
    let mut chars = input.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == close_ch {
            // Skip '.' when it's part of '..' (range operator)
            if close_ch == '.' && input[i + 1..].starts_with('.') {
                chars.next(); // skip the second '.'
                continue;
            }
            depth -= 1;
            if depth == 0 {
                return Some((&input[..i], &input[i + c.len_utf8()..]));
            }
        } else if is_paired && c == open_ch {
            depth += 1;
        } else if c == '<' && input[i + 1..].starts_with('[') {
            // Skip character class <[...]> content without interpreting quotes
            // Handles <['"]>, <[\s]>, etc.
            chars.next(); // consume the '[' we already checked
            let mut bracket_depth = 1u32;
            loop {
                match chars.next() {
                    Some((_, '\\')) => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, ']')) => {
                        bracket_depth -= 1;
                        if bracket_depth == 0 {
                            // Consume the closing >
                            if let Some((_, '>')) = chars.next() {
                                // done
                            }
                            break;
                        }
                    }
                    Some((_, '[')) => {
                        bracket_depth += 1;
                    }
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if c == '<' && !is_paired && !input[i + 1..].starts_with('[') {
            // Track angle bracket nesting for non-paired delimiters (like /).
            // This prevents / inside <:name(/:s .../)> from closing the regex.
            let remaining = &input[i + 1..];
            if remaining.starts_with("?{") || remaining.starts_with("!{") {
                // Code assertion: <?{...}> or <!{...}>
                // Skip the '?' or '!' and then the brace-delimited code block
                chars.next(); // skip ? or !
                chars.next(); // skip {
                let mut brace_depth = 1u32;
                loop {
                    match chars.next() {
                        Some((_, '{')) => brace_depth += 1,
                        Some((_, '}')) => {
                            brace_depth -= 1;
                            if brace_depth == 0 {
                                break;
                            }
                        }
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
                // Consume the closing >
                if let Some((_, '>')) = chars.next() {
                    // done
                }
            } else {
                // Named assertions, Unicode props, etc.: track <> depth
                let mut angle_depth = 1u32;
                loop {
                    match chars.next() {
                        Some((_, '<')) => angle_depth += 1,
                        Some((_, '>')) => {
                            angle_depth -= 1;
                            if angle_depth == 0 {
                                break;
                            }
                        }
                        Some((_, '\\')) => {
                            chars.next();
                        }
                        Some(_) => {}
                        None => return None,
                    }
                }
            }
        } else if c == '\'' || c == '"' {
            // Skip quoted string content in regex (e.g., '/' or '\\').
            // This prevents delimiters inside string atoms like m/ "/" ** 2 /
            // from prematurely ending the regex literal.
            let quote = c;
            loop {
                match chars.next() {
                    Some((_, '\\')) => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, ch)) if ch == quote => break,
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if c == '$' && !is_paired {
            // In non-paired delimiters (like /), $ followed by the close
            // delimiter MIGHT be a variable reference ($/ is the match variable)
            // or it might be the end-of-string anchor followed by the closing
            // delimiter. Disambiguate: if $/ is followed by [ or . or < it's
            // the variable; otherwise it's anchor + close.
            let after = &input[i + 1..];
            if after.starts_with(close_ch) {
                let after_delim = &after[close_ch.len_utf8()..];
                if after_delim.starts_with('[')
                    || after_delim.starts_with('.')
                    || after_delim.starts_with('<')
                {
                    chars.next(); // skip the delimiter char (it's part of $/)
                }
            }
        } else if c == '\\' {
            // skip next char
            chars.next();
        }
    }
    None
}

pub(super) fn regex_lit(input: &str) -> PResult<'_, Expr> {
    // rx/pattern/ or rx{pattern}
    if let Ok((rest, _)) = parse_tag(input, "rx") {
        let (open_ch, close_ch, is_paired) = if rest.starts_with('/') {
            ('/', '/', false)
        } else if rest.starts_with('{') {
            ('{', '}', true)
        } else {
            return Err(PError::expected("regex delimiter"));
        };
        let r = &rest[1..];
        if let Some((pattern, rest)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
            return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
        }
        return Err(PError::expected("regex closing delimiter"));
    }

    // ... — stub operator (Yada-Yada)
    // Must be checked before the sequence operator. Only matches as a primary
    // expression (statement start), not in infix position.
    if let Some(r) = input.strip_prefix("...") {
        // Make sure it's not "...^" (sequence exclude-end) — that's handled in infix
        if !r.starts_with('^') {
            let (r, _) = ws(r)?;
            let (r, msg) = if r.starts_with(';')
                || r.is_empty()
                || r.starts_with('}')
                || r.starts_with(')')
                || r.starts_with(',')
            {
                (
                    r,
                    Expr::Literal(Value::Str("Stub code executed".to_string())),
                )
            } else {
                expression(r)?
            };
            return Ok((
                r,
                Expr::Call {
                    name: "__mutsu_stub_die".to_string(),
                    args: vec![msg],
                },
            ));
        }
    }

    // !!! — fatal stub operator
    if let Some(r) = input.strip_prefix("!!!") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (
                r,
                Expr::Literal(Value::Str("Stub code executed".to_string())),
            )
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: "__mutsu_stub_die".to_string(),
                args: vec![msg],
            },
        ));
    }

    // ??? — admonitory stub operator
    if let Some(r) = input.strip_prefix("???") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (
                r,
                Expr::Literal(Value::Str("Stub code executed".to_string())),
            )
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: "__mutsu_stub_warn".to_string(),
                args: vec![msg],
            },
        ));
    }

    // s with arbitrary delimiter: s/pattern/replacement/, s^pattern^replacement^, etc.
    if let Some(after_s) = input.strip_prefix('s')
        && let Some(open_ch) = after_s.chars().next()
    {
        let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
        // Don't treat s.identifier as substitution when the identifier is 2+ chars
        // (likely a method call on bare 's'). Single-char like s.a.b. is still valid regex.
        let looks_like_method = open_ch == '.'
            && after_s.len() > 2
            && after_s[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
            && after_s[2..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-');
        if is_delim && !looks_like_method {
            let (close_ch, is_paired) = match open_ch {
                '{' => ('}', true),
                '[' => (']', true),
                '(' => (')', true),
                '<' => ('>', true),
                other => (other, false),
            };
            let r = &after_s[open_ch.len_utf8()..];
            if let Some((pattern, after_pat)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
                // For paired delimiters, skip optional whitespace and opening delimiter
                let r2 = if is_paired {
                    let (r2, _) = ws(after_pat)?;
                    r2.strip_prefix(open_ch).unwrap_or(r2)
                } else {
                    after_pat
                };
                if let Some((replacement, rest)) = scan_to_delim(r2, open_ch, close_ch, is_paired) {
                    return Ok((
                        rest,
                        Expr::Subst {
                            pattern: pattern.to_string(),
                            replacement: replacement.to_string(),
                        },
                    ));
                }
            }
        }
    }

    // S/pattern/replacement/ — non-destructive substitution
    if let Some(r) = input.strip_prefix("S/") {
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        if end < bytes.len() {
            let r = &r[end + 1..];
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == b'/' {
                    break;
                }
                if rbytes[rend] == b'\\' && rend + 1 < rbytes.len() {
                    rend += 2;
                } else {
                    rend += 1;
                }
            }
            let replacement = &r[..rend];
            let rest = if rend < rbytes.len() {
                &r[rend + 1..]
            } else {
                &r[rend..]
            };
            return Ok((
                rest,
                Expr::NonDestructiveSubst {
                    pattern: pattern.to_string(),
                    replacement: replacement.to_string(),
                },
            ));
        }
    }

    // tr/from/to/ or TR/from/to/
    if let Some(r) = input
        .strip_prefix("tr/")
        .or_else(|| input.strip_prefix("TR/"))
    {
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == b'/' {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let from = &r[..end];
        if end < bytes.len() {
            let r = &r[end + 1..]; // skip middle /
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == b'/' {
                    break;
                }
                if rbytes[rend] == b'\\' && rend + 1 < rbytes.len() {
                    rend += 2;
                } else {
                    rend += 1;
                }
            }
            let to = &r[..rend];
            let rest = if rend < rbytes.len() {
                &r[rend + 1..]
            } else {
                &r[rend..]
            };
            return Ok((
                rest,
                Expr::Transliterate {
                    from: from.to_string(),
                    to: to.to_string(),
                },
            ));
        }
    }

    // m/pattern/ or m{pattern} or m[pattern]
    // m with arbitrary delimiter: m/.../, m{...}, m[...], m^...^, m!...!, etc.
    // Also allow modifiers before delimiter: m:2x/.../, m:x(2)/.../, m:g:i/.../
    if let Some(after_m) = input.strip_prefix('m') {
        let (spec, adverbs) = parse_match_adverbs(after_m)?;
        if let Some(open_ch) = spec.chars().next() {
            let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
            if is_delim {
                let (close_ch, is_paired) = match open_ch {
                    '{' => ('}', true),
                    '[' => (']', true),
                    '(' => (')', true),
                    '<' => ('>', true),
                    other => (other, false),
                };
                let r = &spec[open_ch.len_utf8()..];
                if let Some((pattern, rest)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
                    if adverbs.exhaustive || adverbs.repeat.is_some() {
                        return Ok((
                            rest,
                            Expr::Literal(Value::RegexWithAdverbs {
                                pattern: pattern.to_string(),
                                exhaustive: adverbs.exhaustive,
                                repeat: adverbs.repeat,
                            }),
                        ));
                    }
                    return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
                }
            }
        }
    }

    // Bare /pattern/
    if input.starts_with('/') && !input.starts_with("//") {
        let r = &input[1..];
        if let Some((pattern, rest)) = scan_to_delim(r, '/', '/', false)
            && !pattern.is_empty()
        {
            return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
        }
    }

    Err(PError::expected("regex literal"))
}

/// Parse a version literal: v5.26.1, v6.c, v6c, v6.d.2
pub(super) fn version_lit(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, 'v')?;
    // Must start with a digit (v6c is valid, but vtest is an identifier)
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_digit() {
        return Err(PError::expected("version number"));
    }
    let (rest, version) = take_while1(rest, |c: char| {
        c.is_ascii_alphanumeric() || c == '.' || c == '*' || c == '+' || c == '-' || c == '_'
    })?;
    // Don't consume trailing '.' — it's likely a method call (e.g. v1.2.3.WHAT)
    // But only if the char after '.' is uppercase (method) or not alphanumeric
    let (version, rest) = if let Some(stripped) = version.strip_suffix('.') {
        // Check what follows: if it's an uppercase letter, it's a method call
        let after = &input[1 + version.len()..];
        if after.is_empty()
            || after.starts_with(|c: char| c.is_ascii_uppercase() || !c.is_ascii_alphanumeric())
        {
            (stripped, &input[1 + version.len() - 1..])
        } else {
            (version, rest)
        }
    } else {
        (version, rest)
    };
    let (parts, plus, minus) = Value::parse_version_string(version);
    Ok((rest, Expr::Literal(Value::Version { parts, plus, minus })))
}

/// Parse a topicalized method call: .say, .uc, .defined, etc.
pub(super) fn topic_method_call(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('.') || input.starts_with("..") {
        return Err(PError::expected("topic method call"));
    }
    let r = &input[1..];
    // .() — invoke topic as callable
    if r.starts_with('(') {
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, args) = parse_call_arg_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((
            r,
            Expr::CallOn {
                target: Box::new(Expr::Var("_".to_string())),
                args,
            },
        ));
    }
    // .&foo(...) — call a code object/sub with topic as first arg
    if let Some(r) = r.strip_prefix('&') {
        let (rest, name) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let mut args = vec![Expr::Var("_".to_string())];
        let rest = if rest.starts_with('(') {
            let (rest, _) = parse_char(rest, '(')?;
            let (rest, _) = ws(rest)?;
            let (rest, call_args) = parse_call_arg_list(rest)?;
            args.extend(call_args);
            let (rest, _) = ws(rest)?;
            let (rest, _) = parse_char(rest, ')')?;
            rest
        } else {
            rest
        };
        return Ok((
            rest,
            Expr::CallOn {
                target: Box::new(Expr::CodeVar(name.to_string())),
                args,
            },
        ));
    }
    let (r, modifier) = if let Some(stripped) = r.strip_prefix('^') {
        (stripped, Some('^'))
    } else if let Some(stripped) = r.strip_prefix('?') {
        (stripped, Some('?'))
    } else {
        (r, None)
    };
    let (rest, name) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name,
                args,
                modifier,
            },
        ));
    }
    // Check for colon-arg syntax: .method: arg, arg2
    let (r2, _) = ws(rest)?;
    if r2.starts_with(':') && !r2.starts_with("::") {
        let r3 = &r2[1..];
        let (r3, _) = ws(r3)?;
        let (r3, first_arg) = expression(r3)?;
        let mut args = vec![first_arg];
        let mut r_inner = r3;
        loop {
            let (r4, _) = ws(r_inner)?;
            if !r4.starts_with(',') {
                break;
            }
            let r4 = &r4[1..];
            let (r4, _) = ws(r4)?;
            let (r4, next) = expression(r4)?;
            args.push(next);
            r_inner = r4;
        }
        return Ok((
            r_inner,
            Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name,
                args,
                modifier,
            },
        ));
    }
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name,
            args: Vec::new(),
            modifier,
        },
    ))
}
