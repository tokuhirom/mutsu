//! Regex, version, and topic-method-call literal parsers.
//!
//! Contains three indivisible parsers:
//!   - `regex_lit`          (~800 lines): parses rx//, m//, s///, S///, tr///, ss///, ...
//!   - `version_lit`        (~27 lines):  parses v6.c, v1.2.3, etc.
//!   - `topic_method_call`  (~330 lines): parses .method, .[], .<>, .(), .++, etc.
//!
//! All three exceed or approach the 500-line limit individually but are not
//! decomposable at a function boundary without semantic change.

use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::{consume_unspace, split_angle_words, ws};
use crate::parser::parse_result::{PError, PResult, parse_char, parse_tag, take_while1};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::adverbs::{
    MatchAdverbs, adverbs_need_value, apply_inline_match_adverbs, build_regex_with_adverbs,
    parse_compact_match_adverbs, parse_match_adverbs, regex_adverb_error,
    reject_subst_only_adverbs, reject_trailing_p5_modifiers, validate_regex_pattern_or_perror,
};
use super::call_args::{
    has_unescaped_statement_boundary, parse_call_arg_list, parse_colon_method_arg,
};
use super::scan::{scan_to_delim, scan_to_delim_p5};
use super::subst::{
    build_non_destructive_subst_expr, build_topic_subst_compound_expr, build_topic_subst_expr,
    parse_subst_replacement_expr, try_strip_subst_compound_assign,
};
use super::trans::{parse_trans_adverbs, process_trans_escapes};

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
        // `rx(o)` without whitespace before `(` should be parsed as an
        // identifier call, not a regex with paren delimiters.  Require at
        // least one whitespace or adverb before `(`.
        if rest.starts_with('(') {
            return Err(PError::expected("regex delimiter"));
        }
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
                adverbs.first_match_adverb.as_deref().unwrap_or("g"),
                Some("rx"),
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
    // `ss` additionally allows whitespace before a bracketing delimiter
    // (e.g. `ss (foo) = 'bar'`), since `ss` is always a substitution.
    if let Some(after_ss) = input.strip_prefix("ss")
        && !crate::parser::stmt::simple::is_user_declared_sub("ss")
        && let Some(first_ch) = after_ss.chars().next()
        && (first_ch == ':'
            || (!first_ch.is_alphanumeric() && first_ch != '_' && !first_ch.is_whitespace())
            || (first_ch.is_whitespace()
                && after_ss.trim_start().starts_with(['(', '[', '{', '<'])))
    {
        let (spec, mut adverbs) = if first_ch == ':' {
            parse_match_adverbs(after_ss)?
        } else {
            (after_ss, MatchAdverbs::default())
        };
        // ss implies :ss (:samespace + :sigspace)
        adverbs.samespace = true;
        adverbs.sigspace = true;
        let spec = if first_ch == ':' || first_ch.is_whitespace() {
            ws(spec)?.0
        } else {
            spec
        };
        if let Some(open_ch) = spec.chars().next() {
            let is_delim = !open_ch.is_alphanumeric() && open_ch != '_' && !open_ch.is_whitespace();
            let looks_like_method = open_ch == '.'
                && spec.len() > 2
                && spec[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
                && spec[2..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-');
            if is_delim && !looks_like_method {
                reject_subst_only_adverbs(&adverbs)?;
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
                    // Bracketing `ss[pattern] = replacement` assignment form.
                    if is_paired {
                        let (after_pat_ws, _) = ws(after_pat)?;
                        if let Some(after_eq) = after_pat_ws.strip_prefix('=')
                            && let Ok((rest, replacement)) = parse_subst_replacement_expr(after_eq)
                        {
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
                reject_subst_only_adverbs(&adverbs)?;
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
                        // Reject obsolete Perl 5 trailing flags on substitution
                        // (`s/a/b/i` -> use `s:i/a/b/`), mirroring `m//`.
                        reject_trailing_p5_modifiers(rest)?;
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
                        let (rest, replacement_expr) = expression(after_eq_ws)?;
                        // Perl5 substitutions keep the legacy `$_ = $_.subst(...)`
                        // lowering: the `.subst` closure path binds Perl5 captures
                        // (`$1`, `$0`, ...) correctly per match, which the generic
                        // Subst interpolator cannot reproduce for Perl5 regex.
                        if adverbs.perl5 {
                            return Ok((
                                rest,
                                build_topic_subst_expr(
                                    pattern.to_string(),
                                    replacement_expr,
                                    &adverbs,
                                )?,
                            ));
                        }
                        // Capture the raw replacement source so the substitution
                        // can be compiled to a `Subst` node. Compiling to `Subst`
                        // (rather than `$_ = $_.subst(...)`) makes the expression
                        // value the proper Match / List-of-Match result (so e.g.
                        // `+(s:g[(\w)] = $0 x 2)` yields the match count) and sets
                        // `$/` to a List under `:g`.
                        let consumed = after_eq_ws.len() - rest.len();
                        let raw_replacement = after_eq_ws[..consumed].trim().to_string();
                        let p = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                        validate_regex_pattern_or_perror(&p)?;
                        let pattern = p;
                        // Wrap the RHS expression in a `{...}` closure block so the
                        // substitution-replacement interpolator evaluates it once per
                        // match with `$/`, `$0`, ... bound to that match.
                        let replacement = format!("{{{raw_replacement}}}");
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
                reject_subst_only_adverbs(&adverbs)?;
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
                        // Expression-based replacement (e.g. S[(o)] = $0.uc).
                        let (after_eq_ws, _) = ws(after_eq)?;
                        let (rest, replacement_expr) = expression(after_eq_ws)?;
                        // Perl5 substitutions keep the legacy `.subst` closure
                        // lowering, which binds Perl5 captures (`$1`, `$0`, ...)
                        // per match; the generic interpolator cannot reproduce it.
                        if adverbs.perl5 {
                            return Ok((
                                rest,
                                build_non_destructive_subst_expr(
                                    pattern.to_string(),
                                    replacement_expr,
                                    &adverbs,
                                )?,
                            ));
                        }
                        // Capture the raw replacement source and wrap it in a
                        // `{...}` block so the NonDestructiveSubst interpolator
                        // evaluates it once per match with `$/`, `$0`, ... bound to
                        // that match. Crucially this does NOT rebind `$_` (it stays
                        // the surrounding topic) nor steal the enclosing block's
                        // placeholder parameters (`$^a`) -- both of which the
                        // AnonSub `.subst` lowering would incorrectly do. Mirrors
                        // the destructive `s[pattern] = expr` path above.
                        let consumed = after_eq_ws.len() - rest.len();
                        let raw_replacement = after_eq_ws[..consumed].trim().to_string();
                        let pattern = apply_inline_match_adverbs(pattern.to_string(), &adverbs);
                        validate_regex_pattern_or_perror(&pattern)?;
                        let replacement = format!("{{{raw_replacement}}}");
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
pub(in crate::parser::primary) fn version_lit(input: &str) -> PResult<'_, Expr> {
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
pub(in crate::parser::primary) fn topic_method_call(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('.') || input.starts_with("..") {
        return Err(PError::expected("topic method call"));
    }
    let r = &input[1..];
    // .=method — mutating topic method call: $_ = $_.method(args)
    if let Some(stripped) = r.strip_prefix('=') {
        let (r, _) = ws(stripped)?;
        if let Ok((r, method_name)) = crate::parser::parse_result::take_while1(r, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        }) {
            let method_name = method_name.to_string();
            let r_before_ws = r;
            let (r, _) = ws(r)?;
            // Parse optional args
            let (r, args) = if r.starts_with('(') {
                let (r, _) = parse_char(r, '(')?;
                let (r, _) = ws(r)?;
                let (r, args) = parse_call_arg_list(r)?;
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                (r, args)
            } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
                // Colon-arg form: .=method: arg. The arg list is a comma-list at
                // listop precedence, so each element stops before the loose
                // word-logical ops (`andthen`/`and`/`or`): `.=new: 1 andthen $y`
                // is `(.=new: 1) andthen $y`, not `.=new(1 andthen $y)`.
                let r = &r_before_ws[1..];
                let (r, _) = ws(r)?;
                let (r, first_arg) = crate::parser::expr::listop_arg_expr(r)?;
                let mut args = vec![first_arg];
                let mut r_inner = r;
                loop {
                    let (r2, _) = ws(r_inner)?;
                    if r2.starts_with(':')
                        && !r2.starts_with("::")
                        && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                    {
                        args.push(arg);
                        r_inner = r3;
                        continue;
                    }
                    if !r2.starts_with(',') {
                        r_inner = r2;
                        break;
                    }
                    let r2 = &r2[1..];
                    let (r2, _) = ws(r2)?;
                    let (r2, next) = crate::parser::expr::listop_arg_expr(r2)?;
                    args.push(next);
                    r_inner = r2;
                }
                (r_inner, args)
            } else if r.starts_with(':') && !r.starts_with("::") {
                // Fake-infix adverb form: .=method :key<val>
                let mut args = Vec::new();
                let mut r_inner = r;
                while r_inner.starts_with(':') && !r_inner.starts_with("::") {
                    if let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r_inner) {
                        args.push(arg);
                        let (r3, _) = ws(r2)?;
                        r_inner = r3;
                    } else {
                        break;
                    }
                }
                (r_inner, args)
            } else {
                (r, Vec::new())
            };
            let method_call = Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: crate::symbol::Symbol::intern(&method_name),
                args,
                modifier: None,
                quoted: false,
            };
            // NOTE: a leading-dot `.=meth` in *expression* position (e.g. the RHS
            // of an `andthen`/`orelse` chain) keeps the plain `AssignExpr` form so
            // the chain-retarget mechanism can redirect the implied `$_` to the
            // chain root variable. The whole-container topic write-through (`given
            // @a { .=uc }`) is handled by the *statement*-level `.=` paths via the
            // `__mutsu_topic_dotassign` marker instead.
            return Ok((
                r,
                Expr::AssignExpr {
                    name: "_".to_string(),
                    expr: Box::new(method_call),
                    is_bind: false,
                },
            ));
        }
    }
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
                        is_positional: false,
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
                is_positional: true,
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
                is_positional: false,
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
    // Parse the method name with the proper Raku hyphen rule: a `-` is part of
    // the identifier only when followed by another identifier char, so `.value--`
    // is `.value` followed by postfix `--`, not a method named `value--`.
    let (rest, name) = crate::parser::primary::var::parse_ident_with_hyphens(r)?;
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
