use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::expr::expression;
use super::string::{finalize_interpolation, process_escape_sequence, try_interpolate_var};

/// Flags representing which interpolation modes are active in a Q/q quoting form.
#[derive(Clone, Default)]
pub(super) struct QuoteFlags {
    pub backslash: bool,  // :b — full escape sequences (\n, \t, etc.)
    pub scalar: bool,     // :s — interpolate $variables
    pub array: bool,      // :a — interpolate @variables[]
    pub hash: bool,       // :h — interpolate %variables<>
    pub function: bool,   // :f — interpolate &functions()
    pub closure: bool,    // :c — interpolate {blocks}
    pub execute: bool,    // :x — execute as shell command
    pub words: bool,      // :w — split on whitespace
    pub quotewords: bool, // :ww — split with quote protection
    pub heredoc: bool,    // :to — heredoc
    pub format: bool,     // :o / :format — sprintf format
    pub val: bool,        // :v — produce allomorphic (val) types for words
    pub q_mode: bool,     // :q — single-quote semantics (\\ and \')
    pub qq_mode: bool,    // :qq — double-quote semantics (all interpolation)
    pub quote_open: Option<char>,
    pub quote_close: Option<char>,
}

impl QuoteFlags {
    /// Create flags for Q (no processing).
    pub fn bare_q_big() -> Self {
        Self::default()
    }

    /// Create flags for q (single-quote mode).
    pub fn q_single() -> Self {
        Self {
            q_mode: true,
            ..Default::default()
        }
    }

    /// Create flags for qq (double-quote mode).
    pub fn qq_double() -> Self {
        Self {
            qq_mode: true,
            ..Default::default()
        }
    }

    /// Apply a named adverb.
    pub fn apply_adverb(&mut self, name: &str) {
        match name {
            "b" | "backslash" => self.backslash = true,
            "s" | "scalar" => self.scalar = true,
            "a" | "array" => self.array = true,
            "h" | "hash" => self.hash = true,
            "f" | "function" => self.function = true,
            "c" | "closure" => self.closure = true,
            "x" | "exec" | "execute" => self.execute = true,
            "w" | "words" => self.words = true,
            "ww" | "quotewords" => self.quotewords = true,
            "to" | "heredoc" => self.heredoc = true,
            "o" | "format" => self.format = true,
            "v" | "val" => self.val = true,
            "q" | "single" => self.q_mode = true,
            "qq" | "double" => self.qq_mode = true,
            _ => {} // ignore unknown adverbs
        }
    }

    /// Whether full backslash escapes are active.
    pub fn full_backslash(&self) -> bool {
        self.backslash || self.qq_mode
    }

    /// Whether any variable/closure interpolation is active.
    pub fn has_interpolation(&self) -> bool {
        self.scalar || self.array || self.hash || self.function || self.closure || self.qq_mode
    }

    /// Whether scalar interpolation is active.
    fn interp_scalar(&self) -> bool {
        self.scalar || self.qq_mode
    }

    /// Whether array interpolation is active.
    fn interp_array(&self) -> bool {
        self.array || self.qq_mode
    }

    /// Whether hash interpolation is active.
    fn interp_hash(&self) -> bool {
        self.hash || self.qq_mode
    }

    /// Whether function interpolation is active.
    fn interp_function(&self) -> bool {
        self.function || self.qq_mode
    }

    /// Whether closure interpolation is active.
    fn interp_closure(&self) -> bool {
        self.closure || self.qq_mode
    }

    /// Whether this is raw Q mode (no processing at all).
    pub fn is_raw(&self) -> bool {
        !self.full_backslash() && !self.q_mode && !self.has_interpolation()
    }
}

/// Parse colon-separated adverbs from the input string.
/// Returns the remaining input after all adverbs.
/// Example: ":s:b:c" → flags.scalar=true, flags.backslash=true, flags.closure=true
pub(super) fn parse_colon_adverbs<'a>(mut input: &'a str, flags: &mut QuoteFlags) -> &'a str {
    loop {
        // Allow whitespace before each colon (e.g. "q :heredoc :c \"EOF\"")
        input = input.trim_start_matches(' ');
        let Some(r) = input.strip_prefix(':') else {
            break;
        };
        let end = r
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(r.len());
        if end == 0 {
            break;
        }
        let adverb_name = &r[..end];
        flags.apply_adverb(adverb_name);
        input = &r[end..];
    }
    input
}

/// Parse fused adverb letters after Q (e.g., Qs, Qa, Qb, Qh, Qf, Qc, Qx, Qw, Qww).
/// Returns the remaining input after the fused adverb.
pub(super) fn parse_fused_adverbs_big_q<'a>(input: &'a str, flags: &mut QuoteFlags) -> &'a str {
    let not_alnum = |r: &str| r.chars().next().is_some_and(|c| !c.is_alphanumeric());
    // Try multi-char fused adverbs first
    if let Some(r) = input.strip_prefix("ww")
        && not_alnum(r)
    {
        flags.quotewords = true;
        return r;
    }
    // Single-char fused adverbs
    type Setter = fn(&mut QuoteFlags);
    let fused: &[(char, Setter)] = &[
        ('s', |f| f.scalar = true),
        ('a', |f| f.array = true),
        ('b', |f| f.backslash = true),
        ('h', |f| f.hash = true),
        ('f', |f| f.function = true),
        ('c', |f| f.closure = true),
        ('x', |f| f.execute = true),
        ('w', |f| f.words = true),
    ];
    for &(ch, setter) in fused {
        if let Some(r) = input.strip_prefix(ch)
            && not_alnum(r)
        {
            setter(flags);
            return r;
        }
    }
    input
}

/// Parse fused adverb letters after q (e.g., qb, qw, qww).
pub(super) fn parse_fused_adverbs_small_q<'a>(input: &'a str, flags: &mut QuoteFlags) -> &'a str {
    let not_alnum = |r: &str| r.chars().next().is_some_and(|c| !c.is_alphanumeric());
    if let Some(r) = input.strip_prefix("ww")
        && not_alnum(r)
    {
        flags.quotewords = true;
        return r;
    }
    if let Some(r) = input.strip_prefix('w')
        && not_alnum(r)
    {
        flags.words = true;
        return r;
    }
    if let Some(r) = input.strip_prefix('b')
        && not_alnum(r)
    {
        flags.backslash = true;
        return r;
    }
    if let Some(r) = input.strip_prefix('a')
        && not_alnum(r)
    {
        flags.array = true;
        return r;
    }
    input
}

/// Process quoted content according to the given flags.
/// This is the unified content processing function for all Q/q forms.
pub(super) fn process_content_with_flags(content: &str, flags: &QuoteFlags) -> Expr {
    // Raw Q mode — no processing at all
    if flags.is_raw() {
        return Expr::Literal(Value::str(content.to_string()));
    }

    // q mode with only :q (single-quote semantics) — minimal processing
    if flags.q_mode && !flags.full_backslash() && !flags.has_interpolation() {
        return process_q_mode_content(content, flags);
    }

    // General processing with selective interpolation
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        // Backslash handling
        if rest.starts_with('\\') && rest.len() > 1 {
            if flags.full_backslash() {
                if let Some(r) = process_b_mode_escape(rest, &mut current, flags) {
                    rest = r;
                    continue;
                }
                let (c, after_escape) = escaped_char(rest).unwrap();
                if c.is_alphanumeric() || c == '_' {
                    if !current.is_empty() {
                        parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
                    }
                    parts.push(Expr::Call {
                        name: Symbol::intern("__mutsu_unknown_backslash_escape"),
                        args: vec![Expr::Literal(Value::str(c.to_string()))],
                    });
                    let _ = after_escape;
                    return finalize_interpolation(parts, current);
                }
                // Unknown non-word escape in :b mode stays literal.
                current.push('\\');
                current.push(c);
                rest = after_escape;
                continue;
            } else if flags.q_mode {
                // q-mode: handle \\, \', and \qq[]/\q:adverb{} escapes
                if let Some(r) =
                    process_q_escape_in_interpolating(rest, &mut parts, &mut current, flags)
                {
                    rest = r;
                    continue;
                }
                // Not a recognized q escape — keep literal
                let (c, after_escape) = escaped_char(rest).unwrap();
                current.push('\\');
                current.push(c);
                rest = after_escape;
                continue;
            }
        }

        // Closure interpolation
        if flags.interp_closure()
            && rest.starts_with('{')
            && let Some((after, inner)) = parse_braced_interpolation(rest)
            && let Ok((remaining, expr)) = expression(inner.trim())
            && remaining.trim().is_empty()
        {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::str(std::mem::take(&mut current))));
            }
            parts.push(expr);
            rest = after;
            continue;
        }

        // Variable/function interpolation
        if flags.has_interpolation()
            && let Some(r) = try_interpolate_with_flags(rest, &mut parts, &mut current, flags)
        {
            rest = r;
            continue;
        }

        // Regular character
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}

fn escaped_char(rest: &str) -> Option<(char, &str)> {
    let after_backslash = rest.strip_prefix('\\')?;
    let c = after_backslash.chars().next()?;
    Some((c, &after_backslash[c.len_utf8()..]))
}

/// Process content in q mode (single-quote semantics).
/// Only handles \\, \', and \qq[]/\q:adverb{} escapes.
fn process_q_mode_content(content: &str, flags: &QuoteFlags) -> Expr {
    // Check for \qq or \q: escape sequences that need special handling
    if content.contains("\\qq")
        || content.contains("\\q:")
        || content.contains("\\q/")
        || content.contains("\\q[")
        || content.contains("\\q{")
        || content.contains("\\q(")
        || content.contains("\\q<")
        || content.contains("\\q|")
    {
        return process_q_mode_with_escapes(content, flags);
    }
    process_q_mode_with_escapes(content, flags)
}

/// Process q-mode content that contains \qq[] or \q:adverb{} escapes.
fn process_q_mode_with_escapes(content: &str, flags: &QuoteFlags) -> Expr {
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = content;

    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            if let Some(r) = process_q_escape(&mut rest, &mut parts, &mut current) {
                rest = r;
                continue;
            }
            // Not a recognized escape — keep literal backslash and char
            let Some((c, after_escape)) = escaped_char(rest) else {
                break;
            };
            if is_q_literal_escape(c, flags) {
                current.push(c);
                rest = after_escape;
            } else if c == '\\' {
                current.push('\\');
                rest = after_escape;
            } else if c == '\'' {
                current.push('\'');
                rest = after_escape;
            } else {
                current.push('\\');
                current.push(c);
                rest = after_escape;
            }
            continue;
        }

        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    finalize_interpolation(parts, current)
}

/// Handle \qq[...] and \q:adverb{...} escapes.
/// Returns Some(remaining) if an escape was handled.
fn process_q_escape<'a>(
    rest: &mut &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
) -> Option<&'a str> {
    let input = *rest;

    // \qq[...] — interpolate inner content as qq
    if let Some(after_qq) = input.strip_prefix("\\qq")
        && let Some(r) = parse_inline_quote(after_qq, &QuoteFlags::qq_double())
    {
        let (after, expr) = r;
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::str(std::mem::take(current))));
        }
        parts.push(expr);
        return Some(after);
    }

    // \q:adverb{...} — interpolate with specified adverbs
    if let Some(after_q) = input.strip_prefix("\\q") {
        if after_q.starts_with(':') {
            let mut flags = QuoteFlags::q_single();
            let after_adverbs = parse_colon_adverbs(after_q, &mut flags);
            if let Some(r) = parse_inline_quote(after_adverbs, &flags) {
                let (after, expr) = r;
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(after);
            }
        }
        // \q/.../ — plain q (no interpolation)
        let first = after_q.chars().next()?;
        if !first.is_alphanumeric() && !first.is_whitespace() {
            let flags = QuoteFlags::q_single();
            if let Some(r) = parse_inline_quote(after_q, &flags) {
                let (after, expr) = r;
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::str(std::mem::take(current))));
                }
                parts.push(expr);
                return Some(after);
            }
        }
    }

    None
}

/// Parse an inline quote from the given position (after \qq or \q:adverb).
/// Returns (remaining, expr) on success.
fn parse_inline_quote<'a>(input: &'a str, flags: &QuoteFlags) -> Option<(&'a str, Expr)> {
    let first = input.chars().next()?;
    if first.is_alphanumeric() || first.is_whitespace() {
        return None;
    }
    let mut flags = flags.clone();

    let bracket_close = match first {
        '{' => Some('}'),
        '[' => Some(']'),
        '(' => Some(')'),
        '<' => Some('>'),
        _ => super::string::unicode_bracket_close_pub(first),
    };

    flags.quote_open = Some(first);
    flags.quote_close = Some(bracket_close.unwrap_or(first));

    if let Some(close) = bracket_close {
        let (rest, content) = super::string::read_bracketed(input, first, close, true).ok()?;
        let expr = process_content_with_flags(content, &flags);
        return Some((rest, expr));
    }

    // Symmetric delimiter
    let body = &input[first.len_utf8()..];
    let end = body.find(first)?;
    let content = &body[..end];
    let rest = &body[end + first.len_utf8()..];
    let expr = process_content_with_flags(content, &flags);
    Some((rest, expr))
}

/// Process a backslash escape in :b mode (full backslash + optional sigil escaping).
/// Returns Some(remaining) on success.
fn process_b_mode_escape<'a>(
    rest: &'a str,
    current: &mut String,
    flags: &QuoteFlags,
) -> Option<&'a str> {
    let (c, after_escape) = escaped_char(rest)?;

    // Handle backslash-escaped sigils that would otherwise trigger interpolation
    if flags.has_interpolation() {
        match c {
            '$' if flags.interp_scalar() => {
                current.push('$');
                return Some(after_escape);
            }
            '@' if flags.interp_array() => {
                current.push('@');
                return Some(after_escape);
            }
            '%' if flags.interp_hash() => {
                current.push('%');
                return Some(after_escape);
            }
            '&' if flags.interp_function() => {
                current.push('&');
                return Some(after_escape);
            }
            '{' if flags.interp_closure() => {
                current.push('{');
                return Some(after_escape);
            }
            _ => {}
        }
    }

    // Standard escape sequences via existing handler
    if let Some((r, _needs_continue)) =
        process_escape_sequence(rest, current, &['%', '&', '{', '}', '\'', '"'])
    {
        return Some(r);
    }

    // Non-word character: strip backslash (e.g. \| → |)
    if !c.is_alphanumeric() && c != '_' {
        current.push(c);
        return Some(after_escape);
    }

    None
}

/// Handle q-mode escapes (\\ and \') within interpolating q content.
fn process_q_escape_in_interpolating<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
    _flags: &QuoteFlags,
) -> Option<&'a str> {
    let (c, after_escape) = escaped_char(rest)?;
    match c {
        _ if is_q_literal_escape(c, _flags) => {
            current.push(c);
            Some(after_escape)
        }
        'q' => {
            // \qq[...] or \q:adverb{...}
            let mut dummy_rest = rest;
            process_q_escape(&mut dummy_rest, parts, current)
        }
        _ => None,
    }
}

fn is_q_literal_escape(c: char, flags: &QuoteFlags) -> bool {
    c == '\\' || c == '\'' || flags.quote_open == Some(c) || flags.quote_close == Some(c)
}

/// Try interpolation based on quote flags (selective per-sigil).
fn try_interpolate_with_flags<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
    flags: &QuoteFlags,
) -> Option<&'a str> {
    if rest.starts_with('$') && flags.interp_scalar() {
        return try_interpolate_var(rest, parts, current);
    }

    if rest.starts_with('@') && flags.interp_array() {
        if !flags.qq_mode {
            // In selective :a mode, require postcircumfix []
            let after_at = &rest[1..];
            if after_at.starts_with('*') || after_at.starts_with('!') || after_at.starts_with('?') {
                // twigil — skip to name
            } else {
                let name_end = after_at
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(after_at.len());
                if name_end == 0 {
                    return None;
                }
                let after_name = &after_at[name_end..];
                if !after_name.starts_with('[') {
                    return None; // bare @name without postcircumfix
                }
            }
        }
        return try_interpolate_var(rest, parts, current);
    }

    if rest.starts_with('%') && flags.interp_hash() {
        if !flags.qq_mode {
            // In selective :h mode, require postcircumfix <> or {}
            let after_pct = &rest[1..];
            let name_end = after_pct
                .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                .unwrap_or(after_pct.len());
            if name_end == 0 {
                return None;
            }
            let after_name = &after_pct[name_end..];
            if !after_name.starts_with('<') && !after_name.starts_with('{') {
                return None;
            }
        }
        return try_interpolate_var(rest, parts, current);
    }

    if rest.starts_with('&') && flags.interp_function() {
        return try_interpolate_function(rest, parts, current);
    }

    None
}

/// Parse braced interpolation: find matching {} with nesting support.
fn parse_braced_interpolation(input: &str) -> Option<(&str, &str)> {
    if !input.starts_with('{') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, ch) in input.char_indices() {
        if ch == '{' {
            depth += 1;
        } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
                let inner = &input[1..idx];
                let after = &input[idx + 1..];
                return Some((after, inner));
            }
        }
    }
    None
}

/// Interpolate a &function() call.
fn try_interpolate_function<'a>(
    rest: &'a str,
    parts: &mut Vec<Expr>,
    current: &mut String,
) -> Option<&'a str> {
    if !rest.starts_with('&') {
        return None;
    }
    let after_amp = &rest[1..];
    let name_end = after_amp
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(after_amp.len());
    if name_end == 0 {
        return None;
    }
    let name = &after_amp[..name_end];
    let after_name = &after_amp[name_end..];

    // Must be followed by ( to be interpolated
    if !after_name.starts_with('(') {
        return None;
    }

    // Find matching )
    let paren_content = &after_name[1..];
    let mut depth = 1usize;
    let mut end = None;
    for (i, ch) in paren_content.char_indices() {
        if ch == '(' {
            depth += 1;
        }
        if ch == ')' {
            depth -= 1;
            if depth == 0 {
                end = Some(i);
                break;
            }
        }
    }
    let end = end?;

    let args_str = &paren_content[..end];
    let after_paren = &paren_content[end + 1..];

    // Parse arguments
    let args = if args_str.is_empty() {
        vec![]
    } else if let Ok((rem, expr)) = expression(args_str.trim()) {
        if rem.trim().is_empty() {
            vec![expr]
        } else {
            vec![Expr::Literal(Value::str(args_str.to_string()))]
        }
    } else {
        vec![Expr::Literal(Value::str(args_str.to_string()))]
    };

    if !current.is_empty() {
        parts.push(Expr::Literal(Value::str(std::mem::take(current))));
    }
    parts.push(Expr::Call {
        name: Symbol::intern(name),
        args,
    });

    Some(after_paren)
}
