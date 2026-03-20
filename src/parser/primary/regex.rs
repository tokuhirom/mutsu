use std::sync::Arc;

use super::super::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};

use crate::ast::{Expr, Stmt};
use crate::regex_validate::validate_regex_syntax;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, Value};

/// Process escape sequences in a tr/// from/to string.
/// Handles \n, \t, \r, \x.., \o.., \\, etc.
fn process_trans_escapes(raw: &str) -> String {
    let mut result = String::new();
    let mut rest = raw;
    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() >= 2 {
            if let Some((remaining, _)) =
                super::string::process_escape_sequence(rest, &mut result, &[])
            {
                rest = remaining;
            } else {
                // Unknown escape: keep as-is
                result.push('\\');
                rest = &rest[1..];
            }
        } else {
            let ch = rest.chars().next().unwrap();
            result.push(ch);
            rest = &rest[ch.len_utf8()..];
        }
    }
    result
}

/// Validate a regex pattern at parse time, converting any RuntimeError to PError.
fn validate_regex_pattern_or_perror(pattern: &str) -> Result<(), PError> {
    validate_regex_syntax(pattern).map_err(|e| {
        if let Some(ex) = e.exception {
            PError::fatal_with_exception(e.message, ex)
        } else {
            PError::fatal(e.message)
        }
    })
}

fn regex_adverb_error(adverb: &str, message: impl Into<String>) -> PError {
    let err = RuntimeError::typed_msg("X::Syntax::Regex::Adverb", message);
    let message = err.message;
    let exception = err.exception.expect("typed regex adverb error");
    let _ = adverb;
    PError::fatal_with_exception(message, exception)
}

/// Reject obsolete Perl 5 trailing regex modifiers (e.g., m/pattern/i, m/pattern/g).
/// In Raku, adverbs come before the delimiter (:i, :g), not after.
fn reject_trailing_p5_modifiers(rest: &str) -> Result<(), PError> {
    // Collect consecutive ASCII lowercase letters immediately after closing delimiter
    let modifier_str: String = rest
        .chars()
        .take_while(|c| c.is_ascii_lowercase())
        .collect();
    if !modifier_str.is_empty() {
        // Check if the next char after the modifier letters is NOT alphanumeric/underscore/hyphen
        // (to avoid false positives like m/foo/method where "method" is an identifier)
        let after_mods = &rest[modifier_str.len()..];
        let next_ch = after_mods.chars().next();
        let is_standalone = match next_ch {
            None => true,
            Some(c) => !c.is_ascii_alphanumeric() && c != '_' && c != '-',
        };
        if is_standalone {
            let old = format!("/{}", modifier_str);
            let replacement = match modifier_str.as_str() {
                "m" => "^^ and $$ anchors",
                "s" => ". to match any character",
                "i" => ":i or :ignorecase adverb",
                "x" => ":x or :sigspace adverb",
                "g" => ":g or :global adverb",
                "e" => "an EVAL block",
                _ => "a Raku adverb",
            };
            let err = RuntimeError::obsolete(&old, replacement);
            return Err(if let Some(ex) = err.exception {
                PError::fatal_with_exception(err.message, ex)
            } else {
                PError::fatal(err.message)
            });
        }
    }
    Ok(())
}

use super::super::expr::expression;
use super::super::helpers::{consume_unspace, skip_balanced_parens, split_angle_words, ws};
use super::super::stmt::assign::try_parse_assign_expr;

/// Parse a single argument in colon method-call syntax (.method: arg1, arg2).
/// Tries colonpair first (:name, :$var, :!flag, :0port), then expression.
fn parse_colon_method_arg(input: &str) -> PResult<'_, Expr> {
    if input.starts_with(':')
        && !input.starts_with("::")
        && let Ok(result) = crate::parser::primary::misc::colonpair_expr(input)
    {
        return Ok(result);
    }
    expression(input)
}

#[derive(Default)]
struct MatchAdverbs {
    global: bool,
    exhaustive: bool,
    overlap: bool,
    repeat: Option<usize>,
    ignore_case: bool,
    ignore_mark: bool,
    samemark: bool,
    samecase: bool,
    sigspace: bool,
    samespace: bool,
    ratchet: bool,
    perl5: bool,
    pos: bool,
    continue_: bool,
    nth: Option<String>,
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

        if name == "g" || name == "global" {
            adverbs.global = true;
        } else if name == "ex" || name == "exhaustive" {
            adverbs.exhaustive = true;
        } else if name == "ov" || name == "overlap" {
            adverbs.overlap = true;
        } else if name == "ii" || name == "samecase" {
            adverbs.samecase = true;
            adverbs.ignore_case = true; // :ii implies :i
        } else if name == "i" || name == "ignorecase" {
            adverbs.ignore_case = true;
        } else if name == "m" || name == "ignoremark" {
            adverbs.ignore_mark = true;
        } else if name == "mm" || name == "samemark" {
            adverbs.samemark = true;
            adverbs.ignore_mark = true; // :mm implies :m
        } else if name == "ss" || name == "samespace" {
            adverbs.samespace = true;
            adverbs.sigspace = true; // :ss implies :s
        } else if name == "s" || name == "sigspace" {
            adverbs.sigspace = true;
        } else if name == "r" || name == "ratchet" {
            adverbs.ratchet = true;
        } else if name == "p" || name == "pos" {
            adverbs.pos = true;
        } else if name == "c" || name == "continue" {
            adverbs.continue_ = true;
        } else if name.eq_ignore_ascii_case("p5") || name.eq_ignore_ascii_case("perl5") {
            adverbs.perl5 = true;
        } else if name == "nth" {
            if let Some(raw) = arg {
                adverbs.nth = Some(raw.trim().to_string());
            }
        } else if (name == "th" || name == "st" || name == "nd" || name == "rd")
            && !leading_digits.is_empty()
        {
            adverbs.nth = Some(leading_digits.clone());
        } else if name == "th" {
            if let Some(raw) = arg {
                adverbs.nth = Some(raw.trim().to_string());
            }
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
        } else {
            return Err(regex_adverb_error(
                &name,
                format!("Unsupported regex adverb :{}", name),
            ));
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
                    adverbs.global = true;
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

/// Parse tr/TR adverbs and the opening delimiter.
/// Returns (remaining_after_open, delimiter_char, close_delimiter_char, is_paired, delete, complement, squash).
fn parse_trans_adverbs(input: &str) -> Option<(&str, char, char, bool, bool, bool, bool)> {
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

    let open_ch = rest.chars().next()?;
    if open_ch.is_alphanumeric() || open_ch == '_' || open_ch.is_whitespace() {
        return None;
    }
    let (close_ch, is_paired) = match open_ch {
        '{' => ('}', true),
        '[' => (']', true),
        '(' => (')', true),
        '<' => ('>', true),
        other => (other, false),
    };
    let after_open = &rest[open_ch.len_utf8()..];
    Some((
        after_open, open_ch, close_ch, is_paired, delete, complement, squash,
    ))
}

fn has_unescaped_statement_boundary(input: &str) -> bool {
    let mut escaped = false;
    for ch in input.chars() {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == ';' || ch == '\n' {
            return true;
        }
    }
    false
}

fn parse_subst_replacement_expr(input: &str) -> PResult<'_, String> {
    let (input, _) = ws(input)?;
    let (rest, expr) = super::primary(input)?;
    let replacement = match expr {
        Expr::Literal(value) => value.to_string_value(),
        _ => {
            return Err(PError::expected(
                "literal replacement expression after '=' in substitution",
            ));
        }
    };
    Ok((rest, replacement))
}

/// Try to strip a compound assignment operator (e.g. `+=`, `x=`, `~=`) from the input.
/// Returns the operator string (without `=`) and the remaining input after `=`.
fn try_strip_subst_compound_assign(input: &str) -> Option<(&str, &str)> {
    // Multi-char operators first
    for op in &["**", "//", "||", "&&", "+|", "+&", "+^", "~|", "~&", "~^"] {
        if let Some(rest) = input.strip_prefix(op)
            && let Some(after_eq) = rest.strip_prefix('=')
        {
            return Some((op, after_eq));
        }
    }
    // Single-char operators
    for op in &["+", "-", "*", "/", "~", "%"] {
        if let Some(rest) = input.strip_prefix(op)
            && let Some(after_eq) = rest.strip_prefix('=')
            // Make sure it's not `==`
            && !after_eq.starts_with('=')
        {
            return Some((op, after_eq));
        }
    }
    // Word operators: x=, fromplus=, etc. — any identifier followed by =
    let mut i = 0;
    let bytes = input.as_bytes();
    if i < bytes.len() && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
        i += 1;
        while i < bytes.len()
            && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'-')
        {
            i += 1;
        }
        if i < bytes.len() && bytes[i] == b'=' && (i + 1 >= bytes.len() || bytes[i + 1] != b'=') {
            return Some((&input[..i], &input[i + 1..]));
        }
    }
    None
}

/// Build an expression for `s[pattern] op= value` compound substitution.
/// This is equivalent to `$_.subst(pattern, { $/ op value })` applied to `$_`.
fn build_topic_subst_compound_expr(
    pattern: String,
    op: &str,
    rhs: Expr,
    adverbs: &MatchAdverbs,
) -> Result<Expr, PError> {
    use crate::token_kind::TokenKind;

    let match_var = Expr::Var("/".to_string());

    let op_token = match op {
        "+" => Some(TokenKind::Plus),
        "-" => Some(TokenKind::Minus),
        "*" => Some(TokenKind::Star),
        "/" => Some(TokenKind::Slash),
        "~" => Some(TokenKind::Tilde),
        "x" => Some(TokenKind::Ident("x".to_string())),
        "%" => Some(TokenKind::Percent),
        "**" => Some(TokenKind::StarStar),
        "//" => Some(TokenKind::SlashSlash),
        "||" => Some(TokenKind::OrOr),
        "&&" => Some(TokenKind::AndAnd),
        _ => None,
    };

    // Build: { $/ op rhs }
    // Note: $/ is stored as Var("/") in the AST (the parser strips the sigil)
    let body_expr = if let Some(op_token) = op_token {
        Expr::Binary {
            left: Box::new(match_var),
            op: op_token,
            right: Box::new(rhs),
        }
    } else {
        // User-defined infix operator: call infix:<op>($/, rhs)
        Expr::Call {
            name: Symbol::intern(&format!("infix:<{op}>")),
            args: vec![match_var, rhs],
        }
    };

    let regex_value = Value::Regex(Arc::new(pattern));

    let mut args = vec![
        Expr::Literal(regex_value),
        Expr::AnonSub {
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
        },
    ];
    if adverbs.global {
        args.push(Expr::Literal(Value::Pair(
            "g".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }

    Ok(Expr::AssignExpr {
        name: "_".to_string(),
        expr: Box::new(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern("subst"),
            args,
            modifier: None,
            quoted: false,
        }),
    })
}

fn build_topic_subst_expr(
    pattern: String,
    replacement: Expr,
    adverbs: &MatchAdverbs,
) -> Result<Expr, PError> {
    if adverbs.nth.is_some() || adverbs.repeat.is_some() {
        return Err(PError::expected(
            "s/// replacement expression without :nth or :x",
        ));
    }

    let pattern = if adverbs.perl5 {
        pattern
    } else {
        let p = apply_inline_match_adverbs(pattern, adverbs);
        validate_regex_pattern_or_perror(&p)?;
        p
    };

    let regex_value = if adverbs.perl5 {
        build_regex_with_adverbs(pattern, adverbs)
    } else {
        Value::Regex(Arc::new(pattern))
    };

    let mut args = vec![
        Expr::Literal(regex_value),
        Expr::AnonSub {
            body: vec![Stmt::Expr(replacement)],
            is_rw: false,
        },
    ];
    if adverbs.global {
        args.push(Expr::Literal(Value::Pair(
            "g".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }

    Ok(Expr::AssignExpr {
        name: "_".to_string(),
        expr: Box::new(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern("subst"),
            args,
            modifier: None,
            quoted: false,
        }),
    })
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
    if adverbs.ratchet {
        pattern = format!(":ratchet {pattern}");
    }
    pattern
}

/// Check whether adverbs require a RegexWithAdverbs value (vs plain Regex).
fn adverbs_need_value(adverbs: &MatchAdverbs) -> bool {
    adverbs.global
        || adverbs.exhaustive
        || adverbs.overlap
        || adverbs.repeat.is_some()
        || adverbs.nth.is_some()
        || adverbs.perl5
        || adverbs.pos
        || adverbs.continue_
        || adverbs.ignore_case
        || adverbs.sigspace
        || adverbs.samecase
        || adverbs.samespace
}

/// Build a RegexWithAdverbs Value from parsed adverbs.
fn build_regex_with_adverbs(pattern: String, adverbs: &MatchAdverbs) -> Value {
    Value::RegexWithAdverbs {
        pattern: Arc::new(pattern),
        global: adverbs.global,
        exhaustive: adverbs.exhaustive,
        overlap: adverbs.overlap,
        repeat: adverbs.repeat,
        nth: adverbs.nth.as_ref().map(|s| Arc::new(s.clone())),
        perl5: adverbs.perl5,
        pos: adverbs.pos,
        continue_: adverbs.continue_,
        ignore_case: adverbs.ignore_case,
        sigspace: adverbs.sigspace,
        samecase: adverbs.samecase,
        samespace: adverbs.samespace,
    }
}

/// Parse comma-separated call arguments inside parens.
/// Semicolons act as list-associative separators: each `;`-delimited group
/// is collected into an `Array` node, producing one arg per group.
pub(in crate::parser) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
    fn parse_call_arg_expr(input: &str) -> PResult<'_, Expr> {
        let (rest, expr) =
            if let Ok(result) = crate::parser::primary::misc::reduction_call_style_expr(input) {
                result
            } else if let Ok((rest, assign_expr)) = try_parse_assign_expr(input) {
                // Only take the assignment fast path when it reaches an argument
                // boundary. Otherwise a parenthesized assignment like `($x = 10)`
                // can be consumed too early inside a larger expression.
                let trimmed = rest.trim_start();
                if trimmed.is_empty()
                    || trimmed.starts_with(',')
                    || trimmed.starts_with(')')
                    || trimmed.starts_with(';')
                {
                    (rest, assign_expr)
                } else {
                    expression(input)?
                }
            } else {
                expression(input)?
            };
        // Handle compound assignment on non-variable expressions in argument position
        // (e.g., `* *= 2` creates WhateverCode that mutates via compound assign).
        let (rest_ws, _) = crate::parser::helpers::ws(rest)?;
        if let Some((stripped, op)) = crate::parser::stmt::assign::parse_compound_assign_op(rest_ws)
        {
            let (r, _) = crate::parser::helpers::ws(stripped)?;
            let (r, rhs) = expression(r)?;
            let mut compound_expr = Expr::Binary {
                left: Box::new(expr),
                op: op.token_kind(),
                right: Box::new(rhs),
            };
            // Apply WhateverCode wrapping (e.g., `* *= 2` -> WhateverCode lambda)
            if crate::parser::expr::should_wrap_whatevercode(&compound_expr) {
                compound_expr = crate::parser::expr::wrap_whatevercode(&compound_expr);
            }
            return Ok((r, compound_expr));
        }
        Ok((rest, expr))
    }

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
        } else if !p5_mode && c == '<' && input[i + 1..].starts_with('<') {
            // << is a left word boundary assertion — skip both chars
            chars.next(); // consume second <
        } else if !p5_mode && c == '>' && input[i + 1..].starts_with('>') {
            // >> is a right word boundary assertion — skip both chars
            chars.next(); // consume second >
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
        } else if !p5_mode && (c == '@' || c == '$') && !is_paired {
            // @(...) or $(...) parenthesized expressions inside regex.
            // Track parenthesis depth so that delimiters (like /) inside
            // the expression don't prematurely close the regex.
            let after = &input[i + c.len_utf8()..];
            if after.starts_with('(') {
                chars.next(); // skip '('
                let mut paren_depth = 1u32;
                loop {
                    match chars.next() {
                        Some((_, '(')) => paren_depth += 1,
                        Some((_, ')')) => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
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
        } else if c == '\\' {
            // skip next char
            chars.next();
        }
    }
    None
}

pub(in crate::parser) fn regex_lit(input: &str) -> PResult<'_, Expr> {
    // y/// is obsolete — reject with X::Obsolete
    if input.starts_with("y/")
        || input.starts_with("y[")
        || input.starts_with("y{")
        || input.starts_with("y|")
    {
        return Err(PError::fatal(
            "X::Obsolete: Unsupported use of y///. In Raku please use: tr///.".to_string(),
        ));
    }

    // qr// is obsolete Perl 5 syntax — reject with X::Obsolete
    if input.starts_with("qr/") || input.starts_with("qr{") || input.starts_with("qr[") {
        return Err(PError::fatal(
            "X::Obsolete: Unsupported use of qr for regex quoting. In Raku please use: rx//."
                .to_string(),
        ));
    }

    // rx/pattern/ or rx{pattern}
    if let Ok((rest, _)) = parse_tag(input, "rx") {
        let (spec, adverbs) = parse_match_adverbs(rest)?;
        if adverbs.global
            || adverbs.exhaustive
            || adverbs.overlap
            || adverbs.repeat.is_some()
            || adverbs.nth.is_some()
            || adverbs.pos
            || adverbs.continue_
        {
            return Err(regex_adverb_error(
                "rx",
                "Match-time adverbs are not allowed on rx// regex literals",
            ));
        }
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
            if adverbs_need_value(&adverbs) {
                let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                return Ok((
                    rest,
                    Expr::Literal(build_regex_with_adverbs(pattern, &adverbs)),
                ));
            }
            return Ok((rest, Expr::Literal(Value::regex(pattern.to_string()))));
        }
        return Err(PError::expected("regex closing delimiter"));
    }

    // ... / … — stub operator (Yada-Yada)
    // Must be checked before the sequence operator. Only matches as a primary
    // expression (statement start), not in infix position.
    if let Some(r) = input
        .strip_prefix("...")
        .or_else(|| input.strip_prefix("…"))
    {
        // Make sure it's not "...^"/"…^" (sequence exclude-end) — that's handled in infix.
        if !r.starts_with('^') {
            let (r, _) = ws(r)?;
            let (r, msg) = if r.starts_with(';')
                || r.is_empty()
                || r.starts_with('}')
                || r.starts_with(')')
                || r.starts_with(',')
            {
                (r, Expr::Literal(Value::str_from("Stub code executed")))
            } else {
                expression(r)?
            };
            return Ok((
                r,
                Expr::Call {
                    name: Symbol::intern("__mutsu_stub_die"),
                    args: vec![msg],
                },
            ));
        }
    }

    // !!! — fatal stub operator
    if let Some(r) = input.strip_prefix("!!!") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (r, Expr::Literal(Value::str_from("Stub code executed")))
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: Symbol::intern("__mutsu_stub_die"),
                args: vec![msg],
            },
        ));
    }

    // ??? — admonitory stub operator
    if let Some(r) = input.strip_prefix("???") {
        let (r, _) = ws(r)?;
        let (r, msg) = if r.starts_with(';') || r.is_empty() || r.starts_with('}') {
            (r, Expr::Literal(Value::str_from("Stub code executed")))
        } else {
            expression(r)?
        };
        return Ok((
            r,
            Expr::Call {
                name: Symbol::intern("__mutsu_stub_warn"),
                args: vec![msg],
            },
        ));
    }

    // ss/pattern/replacement/ — shorthand for s:ss/.../.../
    // Also supports adverbs: ss:g/pattern/replacement/
    if let Some(after_ss) = input.strip_prefix("ss")
        && !crate::parser::stmt::simple::is_user_declared_sub("ss")
        && let Some(first_ch) = after_ss.chars().next()
        && (first_ch == ':'
            || (!first_ch.is_alphanumeric() && first_ch != '_' && !first_ch.is_whitespace()))
    {
        let (spec, mut adverbs) = if first_ch == ':' {
            parse_match_adverbs(after_ss)?
        } else {
            (after_ss, MatchAdverbs::default())
        };
        // ss implies :ss (:samespace + :sigspace)
        adverbs.samespace = true;
        adverbs.sigspace = true;
        let spec = if first_ch == ':' { ws(spec)?.0 } else { spec };
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
                let scan_fn = if adverbs.perl5 {
                    scan_to_delim_p5
                } else {
                    scan_to_delim
                };
                if let Some((pattern, after_pat)) = scan_fn(r, open_ch, close_ch, is_paired) {
                    let r2 = if is_paired {
                        let (r2, _) = ws(after_pat)?;
                        r2.strip_prefix(open_ch).unwrap_or(r2)
                    } else {
                        after_pat
                    };
                    let replacement_scan = if is_paired && !r2.starts_with(open_ch) {
                        None
                    } else {
                        scan_to_delim(r2, open_ch, close_ch, is_paired)
                    };
                    if let Some((replacement, rest)) = replacement_scan {
                        let pattern = if adverbs.perl5 {
                            pattern.to_string()
                        } else {
                            let p = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                            validate_regex_pattern_or_perror(&p)?;
                            p
                        };
                        return Ok((
                            rest,
                            Expr::Subst {
                                pattern,
                                replacement: replacement.to_string(),
                                samecase: adverbs.samecase,
                                sigspace: adverbs.sigspace,
                                samemark: adverbs.samemark,
                                samespace: adverbs.samespace,
                                global: adverbs.global,
                                nth: adverbs.nth.clone(),
                                x: adverbs.repeat,
                                perl5: adverbs.perl5,
                            },
                        ));
                    }
                }
            }
        }
    }

    // s with arbitrary delimiter: s/pattern/replacement/, s^pattern^replacement^, etc.
    // Also supports adverbs: s:mm/pattern/replacement/, s:i:g/pattern/replacement/
    // Skip if 's' has been declared as a user sub — UNLESS followed by ':', which is always
    // substitution (per Raku spec: `s:` is always a substitution even when `sub s` exists).
    if let Some(after_s) = input.strip_prefix('s')
        && let Some(first_ch) = after_s.chars().next()
        && (!crate::parser::stmt::simple::is_user_declared_sub("s") || first_ch == ':')
    {
        // Parse optional adverbs between s and delimiter
        let (spec, adverbs) = if first_ch == ':' {
            parse_match_adverbs(after_s)?
        } else {
            (after_s, MatchAdverbs::default())
        };
        // Allow whitespace between adverbs and delimiter (e.g. s:Perl5 /pattern/)
        let spec = if first_ch == ':' { ws(spec)?.0 } else { spec };
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
                let scan_fn = if adverbs.perl5 {
                    scan_to_delim_p5
                } else {
                    scan_to_delim
                };
                if let Some((pattern, after_pat)) = scan_fn(r, open_ch, close_ch, is_paired) {
                    // For paired delimiters, skip optional whitespace and opening delimiter
                    let r2 = if is_paired {
                        let (r2, _) = ws(after_pat)?;
                        r2.strip_prefix(open_ch).unwrap_or(r2)
                    } else {
                        after_pat
                    };
                    let replacement_scan = if is_paired && !r2.starts_with(open_ch) {
                        None
                    } else {
                        scan_to_delim(r2, open_ch, close_ch, is_paired)
                    };
                    if let Some((replacement, rest)) = replacement_scan {
                        if !is_paired
                            && open_ch == '-'
                            && (has_unescaped_statement_boundary(pattern)
                                || has_unescaped_statement_boundary(replacement))
                        {
                            return Err(PError::expected("substitution"));
                        }
                        let pattern = if adverbs.perl5 {
                            pattern.to_string()
                        } else {
                            let p = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                            validate_regex_pattern_or_perror(&p)?;
                            p
                        };
                        return Ok((
                            rest,
                            Expr::Subst {
                                pattern,
                                replacement: replacement.to_string(),
                                samecase: adverbs.samecase,
                                sigspace: adverbs.sigspace,
                                samemark: adverbs.samemark,
                                samespace: adverbs.samespace,
                                global: adverbs.global,
                                nth: adverbs.nth.clone(),
                                x: adverbs.repeat,
                                perl5: adverbs.perl5,
                            },
                        ));
                    }
                    let (after_pat_ws, _) = ws(after_pat)?;
                    // Check for compound assignment: s[pattern] op= value
                    // Try op= forms (+=, -=, x=, ~=, etc.) before bare =
                    if let Some((op_str, after_op_eq)) =
                        try_strip_subst_compound_assign(after_pat_ws)
                    {
                        let (after_eq_ws, _) = ws(after_op_eq)?;
                        let (rest, rhs_expr) = expression(after_eq_ws)?;
                        let pattern_str = if adverbs.perl5 {
                            pattern.to_string()
                        } else {
                            let p = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                            validate_regex_pattern_or_perror(&p)?;
                            p
                        };
                        return Ok((
                            rest,
                            build_topic_subst_compound_expr(
                                pattern_str,
                                op_str,
                                rhs_expr,
                                &adverbs,
                            )?,
                        ));
                    }
                    if let Some(after_eq) = after_pat_ws.strip_prefix('=') {
                        // Try literal replacement first; fall back to expression
                        if let Ok((rest, replacement)) = parse_subst_replacement_expr(after_eq) {
                            if !is_paired
                                && open_ch == '-'
                                && (has_unescaped_statement_boundary(pattern)
                                    || has_unescaped_statement_boundary(&replacement))
                            {
                                return Err(PError::expected("substitution"));
                            }
                            let pattern = if adverbs.perl5 {
                                pattern.to_string()
                            } else {
                                let p = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                                validate_regex_pattern_or_perror(&p)?;
                                p
                            };
                            return Ok((
                                rest,
                                Expr::Subst {
                                    pattern,
                                    replacement,
                                    samecase: adverbs.samecase,
                                    sigspace: adverbs.sigspace,
                                    samemark: adverbs.samemark,
                                    samespace: adverbs.samespace,
                                    global: adverbs.global,
                                    nth: adverbs.nth.clone(),
                                    x: adverbs.repeat,
                                    perl5: adverbs.perl5,
                                },
                            ));
                        }
                        let (after_eq_ws, _) = ws(after_eq)?;
                        let (rest, replacement) = expression(after_eq_ws)?;
                        return Ok((
                            rest,
                            build_topic_subst_expr(pattern.to_string(), replacement, &adverbs)?,
                        ));
                    }
                }
            }
        }
    }

    // S/pattern/replacement/ — non-destructive substitution
    // Supports adverbs before the delimiter: S:i/.../.../
    // Skip if 'S' has been declared as a user type (class/role/grammar) —
    // it should be parsed as a type object, not substitution.
    if let Some(after_s) = input.strip_prefix('S')
        && !crate::parser::stmt::simple::is_user_declared_type("S")
    {
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
                let scan_fn = if adverbs.perl5 {
                    scan_to_delim_p5
                } else {
                    scan_to_delim
                };
                if let Some((pattern, after_pat)) = scan_fn(r, open_ch, close_ch, is_paired) {
                    let r2 = if is_paired {
                        let (r2, _) = ws(after_pat)?;
                        r2.strip_prefix(open_ch).unwrap_or(r2)
                    } else {
                        after_pat
                    };
                    let replacement_scan = if is_paired && !r2.starts_with(open_ch) {
                        None
                    } else {
                        scan_to_delim(r2, open_ch, close_ch, is_paired)
                    };
                    if let Some((replacement, rest)) = replacement_scan {
                        if !is_paired
                            && open_ch == '-'
                            && (has_unescaped_statement_boundary(pattern)
                                || has_unescaped_statement_boundary(replacement))
                        {
                            return Err(PError::expected("substitution"));
                        }
                        let pattern = if adverbs.perl5 {
                            pattern.to_string()
                        } else {
                            apply_inline_match_adverbs(pattern.to_string(), &adverbs)
                        };
                        return Ok((
                            rest,
                            Expr::NonDestructiveSubst {
                                pattern,
                                replacement: replacement.to_string(),
                                samecase: adverbs.samecase,
                                sigspace: adverbs.sigspace,
                                samemark: adverbs.samemark,
                                samespace: adverbs.samespace,
                                global: adverbs.global,
                                nth: adverbs.nth.clone(),
                                x: adverbs.repeat,
                                perl5: adverbs.perl5,
                            },
                        ));
                    }
                    let (after_pat_ws, _) = ws(after_pat)?;
                    if let Some(after_eq) = after_pat_ws.strip_prefix('=') {
                        let (rest, replacement) = parse_subst_replacement_expr(after_eq)?;
                        if !is_paired
                            && open_ch == '-'
                            && (has_unescaped_statement_boundary(pattern)
                                || has_unescaped_statement_boundary(&replacement))
                        {
                            return Err(PError::expected("substitution"));
                        }
                        let pattern = if adverbs.perl5 {
                            pattern.to_string()
                        } else {
                            apply_inline_match_adverbs(pattern.to_string(), &adverbs)
                        };
                        return Ok((
                            rest,
                            Expr::NonDestructiveSubst {
                                pattern,
                                replacement,
                                samecase: adverbs.samecase,
                                sigspace: adverbs.sigspace,
                                samemark: adverbs.samemark,
                                samespace: adverbs.samespace,
                                global: adverbs.global,
                                nth: adverbs.nth.clone(),
                                x: adverbs.repeat,
                                perl5: adverbs.perl5,
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
                    samecase: false,
                    sigspace: false,
                    samemark: false,
                    samespace: false,
                    global: false,
                    nth: None,
                    x: None,
                    perl5: false,
                },
            ));
        }
    }

    // tr[:adverbs]/from/to/ or TR[:adverbs]/from/to/
    // Supports arbitrary delimiters: tr/.../.../  tr|...|...|  tr[...][...]  tr{...}{...}
    let is_tr_upper = input.starts_with("TR");
    if let Some(r) = input
        .strip_prefix("tr")
        .or_else(|| input.strip_prefix("TR"))
        .and_then(parse_trans_adverbs)
    {
        let (r, _open_ch, close_ch, is_paired, delete, complement, squash) = r;
        let close_byte = close_ch as u8;
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == close_byte {
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
            let r = &r[end + 1..]; // skip close delimiter
            // For paired delimiters, skip optional whitespace and open delimiter of second part
            let r = if is_paired {
                let r = r.trim_start();
                if let Some(r2) = r.strip_prefix(|c: char| c == _open_ch) {
                    r2
                } else {
                    r
                }
            } else {
                r
            };
            let mut rend = 0;
            let rbytes = r.as_bytes();
            while rend < rbytes.len() {
                if rbytes[rend] == close_byte {
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
                    from: process_trans_escapes(from),
                    to: process_trans_escapes(to),
                    delete,
                    complement,
                    squash,
                    non_destructive: is_tr_upper,
                },
            ));
        }
    }

    // m/pattern/ or m{pattern} or m[pattern]
    // m with arbitrary delimiter: m/.../, m{...}, m[...], m^...^, m!...!, etc.
    // Also allow modifiers before delimiter: m:2x/.../, m:x(2)/.../, m:g:i/.../
    // Skip if 'm' has been declared as a user sub — it should be parsed as a function call.
    if let Some(after_m) = input.strip_prefix('m')
        && !after_m.starts_with("=>")
        && !crate::parser::stmt::simple::is_user_declared_sub("m")
    {
        let (spec, mut adverbs) = parse_match_adverbs(after_m)?;
        let spec = parse_compact_match_adverbs(spec, &mut adverbs);
        let (spec, _) = ws(spec)?;
        // After stripping whitespace, check if this is a fat arrow pair (e.g., `m => 1000`).
        // The initial `=>` check above only catches `m=>` without whitespace.
        if spec.starts_with("=>") {
            return Err(PError::expected("regex literal"));
        }
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
                    // Disambiguate `m-foo` style identifiers (e.g., user-defined
                    // callable names like `m-bar`) from `m-...-` regex literals.
                    // If the `-`-delimited candidate spans a statement boundary,
                    // treat it as a non-match and let identifier parsing handle it.
                    if !is_paired && open_ch == '-' && has_unescaped_statement_boundary(pattern) {
                        return Err(PError::expected("regex literal"));
                    }
                    // Detect obsolete Perl 5 trailing modifiers (e.g., m/pattern/i)
                    reject_trailing_p5_modifiers(rest)?;
                    let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                    if !adverbs.perl5 {
                        validate_regex_pattern_or_perror(&pattern)?;
                    }
                    // m// always matches against $_ (unlike rx//)
                    let regex_val = if adverbs_need_value(&adverbs) {
                        build_regex_with_adverbs(pattern, &adverbs)
                    } else {
                        Value::regex(pattern)
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
            return Ok((rest, Expr::Literal(Value::regex(pattern.to_string()))));
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

fn parse_topic_brace_index(input: &str) -> PResult<'_, Expr> {
    let (r, first) = expression(input)?;
    let mut current_dim = vec![first];
    let mut dimensions: Vec<Expr> = Vec::new();
    let mut has_semicolons = false;
    let mut r = r;

    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(',') {
            let (r3, _) = parse_char(r2, ',')?;
            let (r3, _) = ws(r3)?;
            let (r3, next) = expression(r3)?;
            current_dim.push(next);
            r = r3;
            continue;
        }
        if r2.starts_with(';') && !r2.starts_with(";;") {
            has_semicolons = true;
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(std::mem::take(&mut current_dim))
            };
            dimensions.push(dim_expr);
            current_dim = Vec::new();
            let (r3, _) = parse_char(r2, ';')?;
            let (r3, _) = ws(r3)?;
            let (r3, next) = expression(r3)?;
            current_dim.push(next);
            r = r3;
            continue;
        }
        if has_semicolons {
            let dim_expr = if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            };
            dimensions.push(dim_expr);
            return Ok((r2, Expr::ArrayLiteral(dimensions)));
        }
        return Ok((
            r2,
            if current_dim.len() == 1 {
                current_dim.remove(0)
            } else {
                Expr::ArrayLiteral(current_dim)
            },
        ));
    }
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
                    Expr::Literal(Value::str(keys[0].to_string()))
                } else {
                    Expr::ArrayLiteral(
                        keys.into_iter()
                            .map(|k| Expr::Literal(Value::str(k.to_string())))
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
    // .[index] — topicalized index access on $_
    if let Some(r) = r.strip_prefix('[') {
        let (r, _) = ws(r)?;
        let (r, index) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        return Ok((
            r,
            Expr::Index {
                target: Box::new(Expr::Var("_".to_string())),
                index: Box::new(index),
            },
        ));
    }
    // .{index} — topicalized hash/associative lookup on $_
    if let Some(r) = r.strip_prefix('{') {
        let (r, _) = ws(r)?;
        let (r, index) = parse_topic_brace_index(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, '}')?;
        return Ok((
            r,
            Expr::Index {
                target: Box::new(Expr::Var("_".to_string())),
                index: Box::new(index),
            },
        ));
    }
    // .&foo(...) / .&foo: ... — call a code object/sub with topic as first arg
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
        } else if let Ok((r2, _)) = ws(rest) {
            if r2.starts_with(':') && !r2.starts_with("::") {
                let r3 = &r2[1..];
                let (r3, _) = ws(r3)?;
                let (r3, first_arg) = expression(r3)?;
                args.push(first_arg);
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
                r_inner
            } else {
                rest
            }
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
    let name = Symbol::intern(name);
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
        let (r3, first_arg) = parse_colon_method_arg(r3)?;
        let mut args = vec![first_arg];
        let mut r_inner = r3;
        loop {
            let (r4, _) = ws(r_inner)?;
            // Adjacent colonpairs without comma
            if r4.starts_with(':')
                && !r4.starts_with("::")
                && let Ok((r5, arg)) = crate::parser::primary::misc::colonpair_expr(r4)
            {
                args.push(arg);
                r_inner = r5;
                continue;
            }
            if !r4.starts_with(',') {
                break;
            }
            let r4 = &r4[1..];
            let (r4, _) = ws(r4)?;
            let (r4, next) = parse_colon_method_arg(r4)?;
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
