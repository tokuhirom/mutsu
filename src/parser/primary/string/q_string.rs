use super::*;
use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;

/// Parse Q quoting with arbitrary delimiters: Q!...!, Q{...}, Q/.../, etc.
/// Q means no interpolation and no escape processing — content is taken verbatim.
pub(crate) fn big_q_string(input: &str) -> PResult<'_, Expr> {
    use crate::parser::primary::quote_adverbs::{
        QuoteFlags, parse_colon_adverbs, parse_fused_adverbs_big_q, process_content_with_flags,
    };

    let rest = input
        .strip_prefix('Q')
        .ok_or_else(|| PError::expected("Q string"))?;

    // Parse fused adverbs (Qs, Qa, Qb, etc.)
    let mut flags = QuoteFlags::bare_q_big();
    let rest = parse_fused_adverbs_big_q(rest, &mut flags);

    // Parse colon-separated adverbs (:b, :s, :q, :qq, :to, etc.)
    let rest = parse_colon_adverbs(rest, &mut flags);

    // Handle :to (heredoc)
    if flags.heredoc {
        return parse_to_heredoc(rest, flags.has_interpolation() || flags.qq_mode);
    }

    // `Q => ...` is the pair key `Q`, not a `Q`-quote with `=` as its delimiter.
    if rest.trim_start_matches(' ').starts_with("=>") {
        return Err(PError::expected("Q string"));
    }

    if let Some((open, close)) = quote_delimiters(rest) {
        flags.quote_open = Some(open);
        flags.quote_close = Some(close);
    }

    // Handle :q/:qq through existing paths when no other adverbs are set
    if flags.qq_mode
        && !flags.backslash
        && !flags.scalar
        && !flags.array
        && !flags.hash
        && !flags.function
        && !flags.closure
        && !flags.execute
    {
        return parse_q_quoted_content(rest, true, false);
    }
    if flags.q_mode
        && !flags.backslash
        && !flags.scalar
        && !flags.array
        && !flags.hash
        && !flags.function
        && !flags.closure
        && !flags.execute
    {
        return parse_q_quoted_content(rest, false, false);
    }

    // Read delimited content
    let escape = flags.q_mode || flags.qq_mode || flags.backslash;
    let (after, content) = read_delimited_content(rest, escape)?;

    // Process content with flags
    if flags.quotewords {
        let items = parse_quotewords_items(content, &flags)?;
        return Ok((after, make_word_result_expr(items)));
    }
    let expr = process_content_with_flags(content, &flags);

    // Apply post-processing (execute, word-split, format)
    apply_post_processing(after, expr, &flags)
}

/// Apply execute, word-split, and format post-processing to a quote expression.
pub(crate) fn apply_post_processing<'a>(
    after: &'a str,
    expr: Expr,
    flags: &crate::parser::primary::quote_adverbs::QuoteFlags,
) -> PResult<'a, Expr> {
    // Wrap in execute if :x
    if flags.execute {
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern("QX"),
                args: vec![expr],
            },
        ));
    }

    // Handle :w/:ww word splitting
    if flags.quotewords
        && let Expr::Literal(Value::Str(ref s)) = expr
    {
        let items = parse_quotewords_items(s, flags)?;
        return Ok((after, make_word_result_expr(items)));
    }

    if flags.words || flags.quotewords {
        if let Expr::Literal(Value::Str(ref s)) = expr {
            let words: Vec<Expr> = if flags.val {
                s.split_whitespace()
                    .map(|w| {
                        Expr::Literal(
                            crate::parser::primary::container::angle_word_value_full_allomorphic(w),
                        )
                    })
                    .collect()
            } else {
                s.split_whitespace()
                    .map(|w| Expr::Literal(Value::str(w.to_string())))
                    .collect()
            };
            return Ok((after, make_word_result_expr(words)));
        }
        let func_name = if flags.val {
            "__mutsu_quotewords_atom"
        } else {
            "__mutsu_words_atom"
        };
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern(func_name),
                args: vec![expr],
            },
        ));
    }

    if flags.format {
        return Ok((
            after,
            Expr::Call {
                name: Symbol::intern("__mutsu_make_format"),
                args: vec![expr],
            },
        ));
    }

    Ok((after, expr))
}

/// Parse q{...}, q[...], q(...), q<...>, q/.../ quoting forms.
pub(crate) fn q_string(input: &str) -> PResult<'_, Expr> {
    use crate::parser::primary::quote_adverbs::{
        QuoteFlags, parse_colon_adverbs, parse_fused_adverbs_small_q, process_content_with_flags,
    };

    if !input.starts_with('q') {
        return Err(PError::expected("q string"));
    }
    let mut after_q = &input[1..];

    if let Some(after_paren) = after_q.strip_prefix('(')
        && !after_paren.starts_with('(')
    {
        return Err(PError::expected("q string"));
    }

    // q:nfc, q:nfd, q:nfkc, q:nfkd — Unicode normalization adverbs
    for nf_form in &[":nfkc", ":nfkd", ":nfc", ":nfd"] {
        if let Some(rest_after_nf) = after_q.strip_prefix(nf_form) {
            return parse_nf_form(rest_after_nf, &nf_form[1..]);
        }
    }

    // Start with q defaults (single-quote semantics)
    let mut flags = QuoteFlags::q_single();

    // Check for qq first (before fused adverbs, since qqww = qq + ww)
    if after_q.starts_with('q') {
        let after_qq = &after_q[1..];
        // qq is valid if followed by delimiter, colon adverb, or known fused adverb
        let qq_valid = after_qq
            .chars()
            .next()
            .is_some_and(|c| !c.is_alphanumeric() && !c.is_whitespace())
            || after_qq.starts_with("ww")
                && after_qq[2..]
                    .chars()
                    .next()
                    .is_some_and(|c| !c.is_alphanumeric())
            || after_qq.starts_with('w')
                && !after_qq.starts_with("ww")
                && after_qq[1..]
                    .chars()
                    .next()
                    .is_some_and(|c| !c.is_alphanumeric());
        if qq_valid {
            flags.q_mode = false;
            flags.qq_mode = true;
            after_q = after_qq;
        }
    }

    // Parse fused adverbs (qw, qww, qb — also qqww, qqw via the qq detection above)
    after_q = parse_fused_adverbs_small_q(after_q, &mut flags);

    // Parse colon-separated adverbs (:b, :s, :a, :h, :f, :c, :x, :w, :ww, :to, etc.)
    after_q = parse_colon_adverbs(after_q, &mut flags);

    // Handle :to (heredoc)
    if flags.heredoc {
        let interpolate = flags.has_interpolation() || flags.qq_mode;
        return parse_to_heredoc_with_flags(after_q, &flags, interpolate);
    }

    // `#` cannot be used as a quoting delimiter (it starts a comment).
    let trimmed_after_q = after_q.trim_start_matches(' ');
    if trimmed_after_q.starts_with('#') {
        return Err(PError::fatal(
            "# cannot be used as a quote delimiter".to_string(),
        ));
    }

    // `q => ...` is the pair key `q`, not a `q`-quote with `=` as its delimiter.
    // (rakudo accepts `=` as a delimiter for `q=foo=`, but `=>` is a fat arrow.)
    if trimmed_after_q.starts_with("=>") {
        return Err(PError::expected("q string"));
    }

    if let Some((open, close)) = quote_delimiters(after_q) {
        flags.quote_open = Some(open);
        flags.quote_close = Some(close);
    }

    // Read delimited content
    let escape = flags.q_mode || flags.qq_mode || flags.backslash;
    let (rest, content) = read_delimited_content(after_q, escape)?;

    // For q-mode with symmetric delimiters, pre-process \<delim> → <delim>
    let delim_char = after_q.trim_start().chars().next().unwrap_or('/');
    let is_symmetric =
        !matches!(delim_char, '{' | '[' | '(' | '<') && unicode_bracket_close(delim_char).is_none();
    if is_symmetric
        && flags.q_mode
        && !flags.qq_mode
        && delim_char != '\''
        && !flags.has_interpolation()
        && !flags.full_backslash()
    {
        let processed = process_q_escapes(content, delim_char);
        let expr = Expr::Literal(Value::str(processed));
        return apply_post_processing(rest, expr, &flags);
    }

    // When delimiter is {}, disable closure interpolation (Raku spec)
    if delim_char == '{' && flags.closure {
        flags.closure = false;
    }
    if delim_char == '{' && flags.qq_mode {
        // qq{...} — disable closure interp by switching to explicit flags
        flags.qq_mode = false;
        flags.scalar = true;
        flags.array = true;
        flags.hash = true;
        flags.function = true;
        flags.backslash = true;
        // closure stays false
    }

    // Process content with flags
    if flags.quotewords {
        let items = parse_quotewords_items(content, &flags)?;
        return Ok((rest, make_word_result_expr(items)));
    }
    let expr = process_content_with_flags(content, &flags);

    // Apply post-processing (execute, word-split, format)
    apply_post_processing(rest, expr, &flags)
}

/// Parse q:nfc/q:nfd/q:nfkc/q:nfkd forms.
pub(crate) fn parse_nf_form<'a>(rest_after_nf: &'a str, nf_name: &str) -> PResult<'a, Expr> {
    let form_upper = nf_name.to_uppercase();
    let (rest, content) = read_delimited_content(rest_after_nf, false)?;
    use unicode_normalization::UnicodeNormalization;
    let normalized: String = match form_upper.as_str() {
        "NFC" => content.nfc().collect(),
        "NFD" => content.nfd().collect(),
        "NFKC" => content.nfkc().collect(),
        _ => content.nfkd().collect(),
    };
    Ok((rest, Expr::Literal(Value::uni(form_upper, normalized))))
}

/// Helper to build the final expression from quoted content.
pub(crate) fn make_q_content_expr(content: &str, is_qq: bool, q_closure_interp: bool) -> Expr {
    if is_qq {
        interpolate_string_content(content)
    } else if q_closure_interp {
        interpolate_string_content_with_modes(content, false, true)
    } else {
        let s = content.replace("\\'", "'").replace("\\\\", "\\");
        Expr::Literal(Value::str(s))
    }
}

pub(crate) fn parse_q_quoted_content(
    input: &str,
    is_qq: bool,
    q_closure_interp: bool,
) -> PResult<'_, Expr> {
    // Allow optional whitespace between q/qq and the delimiter
    let input = input.trim_start_matches(' ');

    // Must be followed by a delimiter
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("q string delimiter"))?;

    // Check for bracket-style delimiter (possibly doubled)
    let bracket_close = match first {
        '{' => Some('}'),
        '[' => Some(']'),
        '(' => Some(')'),
        '<' => Some('>'),
        _ => unicode_bracket_close(first),
    };

    if let Some(close_ch) = bracket_close {
        // Count how many times the open bracket is repeated (e.g. {{ = 2, {{{ = 3)
        let repeat_count = count_repeated_bracket(input, first);
        if repeat_count > 1 {
            // Multi-bracket delimiter (e.g. q{{ ... }}, q[[ ... ]])
            let open_str: String = std::iter::repeat_n(first, repeat_count).collect();
            let close_str: String = std::iter::repeat_n(close_ch, repeat_count).collect();
            let (rest, content) = read_multi_bracketed(input, &open_str, &close_str, true)?;
            return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
        }
        // Single bracket — use read_bracketed for proper nesting
        let (rest, content) = read_bracketed(input, first, close_ch, true)?;
        return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
    }

    // Non-bracket delimiter (e.g. q/.../, q|...|)
    if !first.is_alphanumeric() && !first.is_whitespace() {
        let rest = &input[first.len_utf8()..];
        let end = rest
            .find(first)
            .ok_or_else(|| PError::expected(&format!("closing '{first}'")))?;
        let content = &rest[..end];
        let rest = &rest[end + first.len_utf8()..];
        return Ok((rest, make_q_content_expr(content, is_qq, q_closure_interp)));
    }

    Err(PError::expected("q string delimiter"))
}
