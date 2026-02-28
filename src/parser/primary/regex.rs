use super::super::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};

use crate::ast::Expr;
use crate::regex_validate::validate_regex_syntax;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Validate a regex pattern at parse time, converting any RuntimeError to PError.
fn validate_regex_pattern_or_perror(pattern: &str) -> Result<(), PError> {
    validate_regex_syntax(pattern).map_err(|e| PError::fatal(e.message))
}

use super::super::expr::expression;
use super::super::helpers::{consume_unspace, skip_balanced_parens, split_angle_words, ws};
use super::super::stmt::assign::try_parse_assign_expr;

#[derive(Default)]
struct MatchAdverbs {
    exhaustive: bool,
    repeat: Option<usize>,
    ignore_case: bool,
    ignore_mark: bool,
    samemark: bool,
    sigspace: bool,
    perl5: bool,
    pos: bool,
    continue_: bool,
}

fn is_regex_quote_open(ch: char) -> bool {
    matches!(
        ch,
        '\'' | '"' | '\u{2018}' | '\u{201A}' | '\u{201C}' | '\u{201E}' | '\u{FF62}'
    )
}

fn is_regex_quote_terminator(open: char, ch: char) -> bool {
    match open {
        '\'' => ch == '\'',
        '"' => ch == '"',
        '\u{2018}' => ch == '\u{2019}',                     // ‘...’
        '\u{201A}' => ch == '\u{2019}' || ch == '\u{2018}', // ‚...’ and ‚...‘
        '\u{201C}' => ch == '\u{201D}',                     // “...”
        '\u{201E}' => ch == '\u{201D}',                     // „...”
        '\u{FF62}' => ch == '\u{FF63}',                     // ｢...｣
        _ => false,
    }
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

        if name == "ex"
            || name == "exhaustive"
            || name == "ov"
            || name == "overlap"
            || name == "g"
            || name == "global"
        {
            adverbs.exhaustive = true;
        } else if name == "i" || name == "ignorecase" {
            adverbs.ignore_case = true;
        } else if name == "m" || name == "ignoremark" {
            adverbs.ignore_mark = true;
        } else if name == "mm" || name == "samemark" {
            adverbs.samemark = true;
            adverbs.ignore_mark = true; // :mm implies :m
        } else if name == "s" || name == "sigspace" {
            adverbs.sigspace = true;
        } else if name == "p" || name == "pos" {
            adverbs.pos = true;
        } else if name == "c" || name == "continue" {
            adverbs.continue_ = true;
        } else if name.eq_ignore_ascii_case("p5") {
            adverbs.perl5 = true;
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

fn parse_compact_match_adverbs<'a>(input: &'a str, adverbs: &mut MatchAdverbs) -> &'a str {
    let mut rest = input;
    loop {
        if let Some(r) = rest.strip_prefix("p5") {
            adverbs.perl5 = true;
            rest = r;
            continue;
        }
        if let Some(ch) = rest.chars().next() {
            let consumed = match ch {
                's' => {
                    adverbs.sigspace = true;
                    true
                }
                'i' => {
                    adverbs.ignore_case = true;
                    true
                }
                'g' => {
                    adverbs.exhaustive = true;
                    true
                }
                'm' => {
                    adverbs.ignore_mark = true;
                    true
                }
                'p' => {
                    adverbs.pos = true;
                    true
                }
                'c' => {
                    adverbs.continue_ = true;
                    true
                }
                _ => false,
            };
            if consumed {
                rest = &rest[ch.len_utf8()..];
                continue;
            }
        }
        break;
    }
    rest
}

fn parse_trans_adverbs(input: &str) -> Option<(&str, bool, bool, bool)> {
    let mut rest = input;
    let mut delete = false;
    let mut complement = false;
    let mut squash = false;

    loop {
        let Some(after_colon) = rest.strip_prefix(':') else {
            break;
        };
        let name_len = after_colon
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
            .unwrap_or(after_colon.len());
        if name_len == 0 {
            return None;
        }
        let name = &after_colon[..name_len];
        match name {
            "d" | "delete" => delete = true,
            "c" | "complement" => complement = true,
            "s" | "squash" => squash = true,
            _ => {}
        }
        rest = &after_colon[name_len..];
    }

    let after_slash = rest.strip_prefix('/')?;
    Some((after_slash, delete, complement, squash))
}

fn apply_inline_match_adverbs(mut pattern: String, adverbs: &MatchAdverbs) -> String {
    if adverbs.ignore_case {
        pattern = format!(":i {pattern}");
    }
    if adverbs.ignore_mark {
        pattern = format!(":m {pattern}");
    }
    if adverbs.sigspace {
        pattern = format!(":s {pattern}");
    }
    pattern
}

/// Parse comma-separated call arguments inside parens.
/// Semicolons act as list-associative separators: each `;`-delimited group
/// is collected into an `Array` node, producing one arg per group.
pub(in crate::parser) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
    fn parse_call_arg_expr(input: &str) -> PResult<'_, Expr> {
        crate::parser::primary::misc::reduction_call_style_expr(input)
            .or_else(|_| try_parse_assign_expr(input))
            .or_else(|_| expression(input))
    }

    let (input, _) = ws(input)?;
    if input.starts_with(')') {
        return Ok((input, Vec::new()));
    }
    let (input, first) = parse_call_arg_expr(input)?;
    let mut current_group = vec![first];
    let mut groups: Vec<Vec<Expr>> = Vec::new();
    let mut has_semicolon = false;
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with(';') && !r.starts_with(";;") {
            // Semicolon separator: finish current group, start new one
            has_semicolon = true;
            groups.push(std::mem::take(&mut current_group));
            let r = &r[1..];
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                // Trailing semicolon before close paren
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            let (r, arg) = parse_call_arg_expr(r)?;
            current_group.push(arg);
            rest = r;
            continue;
        }
        // Adjacent colonpairs without commas: foo(:a :b :c) or foo(:a:b:c)
        if r.starts_with(':')
            && !r.starts_with("::")
            && let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r)
        {
            current_group.push(arg);
            rest = r2;
            continue;
        }
        if !r.starts_with(',') {
            if has_semicolon {
                groups.push(std::mem::take(&mut current_group));
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            return Ok((r, current_group));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            if has_semicolon {
                groups.push(std::mem::take(&mut current_group));
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            return Ok((r, current_group));
        }
        let (r, arg) = parse_call_arg_expr(r)?;
        current_group.push(arg);
        rest = r;
    }
}

/// Convert semicolon-separated groups into Array args.
fn semicolon_groups_to_args(groups: Vec<Vec<Expr>>, _empty: Vec<Expr>) -> Vec<Expr> {
    use crate::ast::Expr;
    groups
        .into_iter()
        .map(|g| {
            if g.len() == 1 {
                // Single-element group: wrap in array for consistency
                Expr::ArrayLiteral(g)
            } else {
                Expr::ArrayLiteral(g)
            }
        })
        .collect()
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
    scan_to_delim_inner(input, open_ch, close_ch, is_paired, false)
}

/// Like `scan_to_delim` but with an option to skip Raku-specific handling
/// (angle brackets, single-quoted strings, `$` variable detection).
/// In P5 mode, only backslash escapes and the close delimiter are significant.
pub(in crate::parser) fn scan_to_delim_p5(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
) -> Option<(&str, &str)> {
    scan_to_delim_inner(input, open_ch, close_ch, is_paired, true)
}

fn scan_to_delim_inner(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
    p5_mode: bool,
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
        } else if !p5_mode && c == '<' && input[i + 1..].starts_with('[') {
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
        } else if !p5_mode
            && c == '<'
            && !is_paired
            && !input[i + 1..].starts_with('[')
            && !input[i + 1..].starts_with('(')
        {
            // Track angle bracket nesting for non-paired delimiters (like /).
            // This prevents / inside <:name(/:s .../)> from closing the regex.
            let remaining = &input[i + 1..];
            if remaining.starts_with("?{")
                || remaining.starts_with("!{")
                || remaining.starts_with('{')
            {
                // Code assertion/interpolation: <?{...}>, <!{...}>, or <{...}>
                // Skip the '?' or '!' prefix if present, then the brace-delimited block
                if remaining.starts_with("?{") || remaining.starts_with("!{") {
                    chars.next(); // skip ? or !
                }
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
        } else if !p5_mode && is_regex_quote_open(c) {
            // Skip quoted string content in regex (e.g., '/' or '\\').
            // This prevents delimiters inside string atoms like m/ "/" ** 2 /
            // from prematurely ending the regex literal.
            loop {
                match chars.next() {
                    Some((_, '\\')) => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, ch)) if is_regex_quote_terminator(c, ch) => break,
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if !p5_mode && c == '$' && !is_paired {
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
        let (spec, adverbs) = parse_match_adverbs(rest)?;
        let (spec, _) = ws(spec)?;
        let (open_ch, close_ch, is_paired) = if spec.starts_with('/') {
            ('/', '/', false)
        } else if spec.starts_with('{') {
            ('{', '}', true)
        } else if spec.starts_with('[') {
            ('[', ']', true)
        } else if spec.starts_with('(') {
            ('(', ')', true)
        } else if spec.starts_with('<') {
            ('<', '>', true)
        } else {
            return Err(PError::expected("regex delimiter"));
        };
        let r = &spec[1..];
        let scan_result = if adverbs.perl5 {
            scan_to_delim_p5(r, open_ch, close_ch, is_paired)
        } else {
            scan_to_delim(r, open_ch, close_ch, is_paired)
        };
        if let Some((pattern, rest)) = scan_result {
            if !adverbs.perl5 {
                validate_regex_pattern_or_perror(pattern)?;
            }
            let has_adverbs = adverbs.exhaustive
                || adverbs.repeat.is_some()
                || adverbs.perl5
                || adverbs.pos
                || adverbs.continue_;
            if has_adverbs {
                let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                return Ok((
                    rest,
                    Expr::Literal(Value::RegexWithAdverbs {
                        pattern,
                        exhaustive: adverbs.exhaustive,
                        repeat: adverbs.repeat,
                        perl5: adverbs.perl5,
                        pos: adverbs.pos,
                    }),
                ));
            }
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
    // Also supports adverbs: s:mm/pattern/replacement/, s:i:g/pattern/replacement/
    // Skip if 's' has been declared as a user sub — it should be parsed as a function call.
    if let Some(after_s) = input.strip_prefix('s')
        && !crate::parser::stmt::simple::is_user_declared_sub("s")
        && let Some(first_ch) = after_s.chars().next()
    {
        // Parse optional adverbs between s and delimiter
        let (spec, adverbs) = if first_ch == ':' {
            parse_match_adverbs(after_s)?
        } else {
            (after_s, MatchAdverbs::default())
        };
        if let Some(open_ch) = spec.chars().next() {
            let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
            // Don't treat s.identifier as substitution when the identifier is 2+ chars
            // (likely a method call on bare 's'). Single-char like s.a.b. is still valid regex.
            let looks_like_method = open_ch == '.'
                && spec.len() > 2
                && spec[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
                && spec[2..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-');
            if is_delim && !looks_like_method {
                let (close_ch, is_paired) = match open_ch {
                    '{' => ('}', true),
                    '[' => (']', true),
                    '(' => (')', true),
                    '<' => ('>', true),
                    other => (other, false),
                };
                let r = &spec[open_ch.len_utf8()..];
                if let Some((pattern, after_pat)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
                    // For paired delimiters, skip optional whitespace and opening delimiter
                    let r2 = if is_paired {
                        let (r2, _) = ws(after_pat)?;
                        r2.strip_prefix(open_ch).unwrap_or(r2)
                    } else {
                        after_pat
                    };
                    if let Some((replacement, rest)) =
                        scan_to_delim(r2, open_ch, close_ch, is_paired)
                    {
                        let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                        validate_regex_pattern_or_perror(&pattern)?;
                        return Ok((
                            rest,
                            Expr::Subst {
                                pattern,
                                replacement: replacement.to_string(),
                                samemark: adverbs.samemark,
                            },
                        ));
                    }
                }
            }
        }
    }

    // S/pattern/replacement/ — non-destructive substitution
    // Supports adverbs before the delimiter: S:i/.../.../
    if let Some(after_s) = input.strip_prefix('S') {
        let (spec, adverbs) = parse_match_adverbs(after_s)?;
        if let Some(open_ch) = spec.chars().next() {
            let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
            let looks_like_method = open_ch == '.'
                && spec.len() > 2
                && spec[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
                && spec[2..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-');
            if is_delim && !looks_like_method {
                let (close_ch, is_paired) = match open_ch {
                    '{' => ('}', true),
                    '[' => (']', true),
                    '(' => (')', true),
                    '<' => ('>', true),
                    other => (other, false),
                };
                let r = &spec[open_ch.len_utf8()..];
                if let Some((pattern, after_pat)) = scan_to_delim(r, open_ch, close_ch, is_paired) {
                    let r2 = if is_paired {
                        let (r2, _) = ws(after_pat)?;
                        r2.strip_prefix(open_ch).unwrap_or(r2)
                    } else {
                        after_pat
                    };
                    if let Some((replacement, rest)) =
                        scan_to_delim(r2, open_ch, close_ch, is_paired)
                    {
                        let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                        return Ok((
                            rest,
                            Expr::NonDestructiveSubst {
                                pattern,
                                replacement: replacement.to_string(),
                                samemark: adverbs.samemark,
                            },
                        ));
                    }
                }
            }
        }
    }

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
                    samemark: false,
                },
            ));
        }
    }

    // tr[:adverbs]/from/to/ or TR[:adverbs]/from/to/
    if let Some(r) = input
        .strip_prefix("tr")
        .or_else(|| input.strip_prefix("TR"))
        .and_then(parse_trans_adverbs)
    {
        let (r, delete, complement, squash) = r;
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
                    delete,
                    complement,
                    squash,
                },
            ));
        }
    }

    // m/pattern/ or m{pattern} or m[pattern]
    // m with arbitrary delimiter: m/.../, m{...}, m[...], m^...^, m!...!, etc.
    // Also allow modifiers before delimiter: m:2x/.../, m:x(2)/.../, m:g:i/.../
    // Skip if 'm' has been declared as a user sub — it should be parsed as a function call.
    if let Some(after_m) = input.strip_prefix('m')
        && !crate::parser::stmt::simple::is_user_declared_sub("m")
    {
        let (spec, mut adverbs) = parse_match_adverbs(after_m)?;
        let spec = parse_compact_match_adverbs(spec, &mut adverbs);
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
                let scan_result = if adverbs.perl5 {
                    scan_to_delim_p5(r, open_ch, close_ch, is_paired)
                } else {
                    scan_to_delim(r, open_ch, close_ch, is_paired)
                };
                if let Some((pattern, rest)) = scan_result {
                    let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                    if !adverbs.perl5 {
                        validate_regex_pattern_or_perror(&pattern)?;
                    }
                    let has_adverbs = adverbs.exhaustive
                        || adverbs.repeat.is_some()
                        || adverbs.perl5
                        || adverbs.pos
                        || adverbs.continue_;
                    // m// always matches against $_ (unlike rx//)
                    let regex_val = if has_adverbs {
                        Value::RegexWithAdverbs {
                            pattern,
                            exhaustive: adverbs.exhaustive,
                            repeat: adverbs.repeat,
                            perl5: adverbs.perl5,
                            pos: adverbs.pos,
                        }
                    } else {
                        Value::Regex(pattern)
                    };
                    return Ok((rest, Expr::MatchRegex(regex_val)));
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
            validate_regex_pattern_or_perror(pattern)?;
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
    // .++ and .-- — postfix increment/decrement on $_
    if let Some(rest) = r.strip_prefix("++") {
        return Ok((
            rest,
            Expr::PostfixOp {
                op: TokenKind::PlusPlus,
                expr: Box::new(Expr::Var("_".to_string())),
            },
        ));
    }
    if let Some(rest) = r.strip_prefix("--") {
        return Ok((
            rest,
            Expr::PostfixOp {
                op: TokenKind::MinusMinus,
                expr: Box::new(Expr::Var("_".to_string())),
            },
        ));
    }
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
    // .<key> topical hash/associative lookup: equivalent to $_<key>
    if r.starts_with('<') && !r.starts_with("<=") && !r.starts_with("<<") && !r.starts_with("<=>") {
        let r2 = &r[1..];
        if let Some(end) = r2.find('>') {
            let content = &r2[..end];
            let keys = split_angle_words(content);
            if !keys.is_empty()
                && keys.iter().all(|key| {
                    !key.is_empty()
                        && key.chars().all(|c| {
                            c.is_alphanumeric()
                                || c == '_'
                                || c == '-'
                                || c == '!'
                                || c == '.'
                                || c == ':'
                                || c == '?'
                                || c == '+'
                                || c == '/'
                                || c == '$'
                                || c == '@'
                                || c == '%'
                                || c == '&'
                        })
                })
            {
                let rest = &r2[end + 1..];
                let index_expr = if keys.len() == 1 {
                    Expr::Literal(Value::Str(keys[0].to_string()))
                } else {
                    Expr::ArrayLiteral(
                        keys.into_iter()
                            .map(|k| Expr::Literal(Value::Str(k.to_string())))
                            .collect(),
                    )
                };
                return Ok((
                    rest,
                    Expr::Index {
                        target: Box::new(Expr::Var("_".to_string())),
                        index: Box::new(index_expr),
                    },
                ));
            }
        }
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
    // Detect illegal space between method name and parens: .method (args) is Confused
    // Raku requires no space between method name and opening paren.
    if (rest.starts_with(' ') || rest.starts_with('\t')) && !rest.starts_with('\\') {
        let after_ws = rest.trim_start_matches([' ', '\t']);
        if after_ws.starts_with('(') {
            return Err(PError::expected_at(
                "Confused. no space allowed between method name and the left parenthesis",
                rest,
            ));
        }
    }
    // Handle unspace between method name and parens: .method\ (args)
    let rest = consume_unspace(rest);
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
                quoted: false,
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
                quoted: false,
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
            quoted: false,
        },
    ))
}
