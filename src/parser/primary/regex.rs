use super::super::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};

use crate::ast::Expr;
use crate::value::Value;

use super::super::expr::expression;
use super::super::helpers::ws;

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
pub(super) fn scan_to_delim(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    let mut depth = 1u32;
    let mut chars = input.char_indices();
    while let Some((i, c)) = chars.next() {
        if c == close_ch {
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
        } else if c == '\'' {
            // Skip single-quoted string content in regex (e.g., '/' or '\\')
            loop {
                match chars.next() {
                    Some((_, '\\')) => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, '\'')) => break,
                    Some(_) => {}
                    None => return None,
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
                name: "die".to_string(),
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
                name: "warn".to_string(),
                args: vec![msg],
            },
        ));
    }

    // s with arbitrary delimiter: s/pattern/replacement/, s^pattern^replacement^, etc.
    if let Some(after_s) = input.strip_prefix('s')
        && let Some(open_ch) = after_s.chars().next()
    {
        let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
        if is_delim {
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
    if let Some(after_m) = input.strip_prefix('m')
        && let Some(open_ch) = after_m.chars().next()
    {
        let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
        if is_delim {
            let (close_ch, is_paired) = match open_ch {
                '{' => ('}', true),
                '[' => (']', true),
                '(' => (')', true),
                '<' => ('>', true),
                other => (other, false),
            };
            let r = &after_m[open_ch.len_utf8()..];
            if let Some((pattern, rest)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
                return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
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

/// Parse a version literal: v5.26.1
pub(super) fn version_lit(input: &str) -> PResult<'_, Expr> {
    use crate::value::VersionPart;
    let (rest, _) = parse_char(input, 'v')?;
    // Must start with a digit
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_digit() {
        return Err(PError::expected("version number"));
    }
    let (rest, version) = take_while1(rest, |c: char| {
        c.is_ascii_digit() || c == '.' || c == '*' || c == '+' || c == '-'
    })?;
    // Don't consume trailing '.' — it's likely a method call (e.g. v1.2.3.WHAT)
    let (version, rest) = if let Some(stripped) = version.strip_suffix('.') {
        (stripped, &input[1 + version.len() - 1..])
    } else {
        (version, rest)
    };
    // Check for + or - suffix
    let version_str = version.trim_end_matches(['+', '-']);
    let plus = version.ends_with('+');
    let minus = version.ends_with('-');
    let parts: Vec<VersionPart> = version_str
        .split('.')
        .map(|s| {
            if s == "*" {
                VersionPart::Whatever
            } else {
                VersionPart::Num(s.parse::<i64>().unwrap_or(0))
            }
        })
        .collect();
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
