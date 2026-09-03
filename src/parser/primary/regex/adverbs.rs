//! Adverb parsing, validation, and regex-with-adverbs building.

use std::sync::Arc;

use crate::parser::helpers::skip_balanced_parens;
use crate::parser::parse_result::{PError, PResult};
use crate::runtime::regex_parse::validate_regex_structurally;
use crate::symbol::Symbol;
use crate::value::Value;

/// Validate a regex pattern at parse time, converting any RuntimeError to PError.
pub(super) fn validate_regex_pattern_or_perror(pattern: &str) -> Result<(), PError> {
    validate_regex_structurally(pattern).map_err(|e| {
        if let Some(ex) = e.exception {
            PError::fatal_with_exception(e.message.into_owned(), ex)
        } else {
            PError::fatal(e.message.into_owned())
        }
    })
}

pub(super) fn regex_adverb_error(
    adverb: &str,
    construct: Option<&str>,
    message: impl Into<String>,
) -> PError {
    let message = message.into();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("adverb".to_string(), Value::str(adverb.to_string()));
    if let Some(c) = construct {
        attrs.insert("construct".to_string(), Value::str(c.to_string()));
    }
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Adverb"), attrs);
    PError::fatal_with_exception(message, Box::new(ex))
}

/// Adverbs parsed from a match/subst/rx expression.
#[derive(Default)]
pub(super) struct MatchAdverbs {
    pub(super) global: bool,
    pub(super) exhaustive: bool,
    pub(super) overlap: bool,
    /// Raw `:x` adverb argument spec: a count (`"3"`) or a range (`"1..3"`),
    /// kept as a string so the substitution can support range arguments.
    pub(super) repeat: Option<String>,
    pub(super) ignore_case: bool,
    pub(super) ignore_mark: bool,
    pub(super) samemark: bool,
    pub(super) samecase: bool,
    pub(super) sigspace: bool,
    pub(super) samespace: bool,
    pub(super) ratchet: bool,
    pub(super) perl5: bool,
    pub(super) pos: bool,
    /// Literal argument of `:pos(N)` (anchor position), if given.
    pub(super) pos_value: Option<usize>,
    pub(super) continue_: bool,
    /// Literal argument of `:continue(N)` / `:c(N)` (search-from position), if given.
    pub(super) continue_value: Option<usize>,
    pub(super) nth: Option<String>,
    /// The first match-time adverb name as written by the user (`g`, `global`,
    /// `ov`, `ex`, ...). Used to report X::Syntax::Regex::Adverb.adverb when a
    /// match-time adverb appears where it is not allowed (rx// / substitution).
    pub(super) first_match_adverb: Option<String>,
}

/// True for boolean-switch regex adverbs that take no runtime value (unlike the
/// value adverbs `:x`, `:nth`, `:pos`, `:continue`, `:g`).
pub(super) fn is_boolean_flag_adverb(name: &str) -> bool {
    matches!(
        name,
        "i" | "ignorecase"
            | "ii"
            | "samecase"
            | "s"
            | "sigspace"
            | "ss"
            | "samespace"
            | "m"
            | "ignoremark"
            | "mm"
            | "samemark"
            | "r"
            | "ratchet"
            | "ex"
            | "exhaustive"
            | "ov"
            | "overlap"
    )
}

/// True when a boolean-flag adverb argument is not a compile-time constant.
/// These adverbs (`:i`, `:m`, `:s`, ...) are compile-time switches, so their
/// value must be a literal (`:i(1)`, `:i(True)`) — any variable reference
/// (`:i($i)`, `:i(@*ARGS[0])`, `:i($obj.attr)`) is X::Value::Dynamic. We detect
/// non-constness by the presence of a sigil that begins a variable name (a
/// regular or twigil'd lexical/dynamic/attribute variable).
fn contains_dynamic_var(src: &str) -> bool {
    let bytes = src.as_bytes();
    bytes.windows(2).any(|w| {
        matches!(w[0], b'$' | b'@' | b'%' | b'&')
            && (w[1].is_ascii_alphabetic()
                || matches!(w[1], b'_' | b'*' | b'!' | b'.' | b'^' | b':' | b'<'))
    })
}

/// Reject adverbs that make no sense on a substitution. `:overlap`/`:ov` and
/// `:exhaustive`/`:ex` are only meaningful for matching (there is no way to
/// substitute overlapping matches), so Raku rejects them at compile time with
/// X::Syntax::Regex::Adverb.
pub(super) fn reject_subst_only_adverbs(adverbs: &MatchAdverbs) -> Result<(), PError> {
    if adverbs.overlap {
        return Err(regex_adverb_error(
            adverbs.first_match_adverb.as_deref().unwrap_or("overlap"),
            Some("substitution"),
            "Adverb overlap not allowed on substitution",
        ));
    }
    if adverbs.exhaustive {
        return Err(regex_adverb_error(
            adverbs
                .first_match_adverb
                .as_deref()
                .unwrap_or("exhaustive"),
            Some("substitution"),
            "Adverb exhaustive not allowed on substitution",
        ));
    }
    Ok(())
}

/// Reject obsolete Perl 5 trailing regex modifiers (e.g., m/pattern/i, m/pattern/g).
/// In Raku, adverbs come before the delimiter (:i, :g), not after.
pub(super) fn reject_trailing_p5_modifiers(rest: &str) -> Result<(), PError> {
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
            let err = crate::value::RuntimeError::obsolete(&old, replacement);
            return Err(if let Some(ex) = err.exception {
                PError::fatal_with_exception(err.message.into_owned(), ex)
            } else {
                PError::fatal(err.message.into_owned())
            });
        }
    }
    Ok(())
}

/// Raku allows whitespace between a quote-like operator and its adverbs, and
/// between consecutive adverbs (`s :g :i/pat/repl/`, `m :i /B/`, `tr :d/b//`).
/// Returns `input` with that leading whitespace stripped, but ONLY when an
/// adverb really follows — a plain `s /pat/repl/` (whitespace before the
/// *delimiter*) must be left untouched, because the callers use the presence of
/// that whitespace to decide whether `(` opens a delimiter or a call.
pub(super) fn skip_ws_before_adverb(input: &str) -> Option<&str> {
    let trimmed = input.trim_start();
    if trimmed.len() == input.len() {
        return None;
    }
    let rest = trimmed.strip_prefix(':')?;
    let c = rest.chars().next()?;
    // `::Foo` is a package name, never an adverb; an adverb name starts with an
    // identifier character (`:g`, `:samecase`) or a digit (`:2nd`).
    (c.is_ascii_alphanumeric() || c == '_').then_some(trimmed)
}

/// True when `input` opens with an adverb, with or without leading whitespace.
pub(super) fn starts_with_adverb(input: &str) -> bool {
    input.starts_with(':') || skip_ws_before_adverb(input).is_some()
}

pub(super) fn parse_match_adverbs(input: &str) -> PResult<'_, MatchAdverbs> {
    let mut spec = input;
    let mut adverbs = MatchAdverbs::default();
    loop {
        if !spec.starts_with(':') {
            match skip_ws_before_adverb(spec) {
                Some(after_ws) => spec = after_ws,
                None => break,
            }
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

        // A value-taking match adverb (`:nth`, `:th`, `:x`) takes its argument
        // in parens only. Square brackets are regex *delimiters*, so
        // `m:nth[5]/fo+/` would read `[5]` as the pattern and choke on the
        // trailing `/fo+/` — Rakudo reports the cascade as X::Comp::Group.
        // Reject the bracket form directly with the same exception type.
        if arg.is_none()
            && leading_digits.is_empty()
            && matches!(name.as_str(), "nth" | "st" | "nd" | "rd" | "th" | "x")
            && r.starts_with('[')
        {
            let message = format!(
                "Adverb :{} takes its argument in parentheses, not square brackets",
                name
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Comp::Group"), attrs);
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }

        // A boolean-flag regex adverb (`:i`, `:s`, `:m`, ...) is a compile-time
        // switch and cannot take a runtime value. When such an adverb is given an
        // argument that references a dynamic variable (e.g. `m:i(@*ARGS[0])/`),
        // it is X::Value::Dynamic ("Adverb i value must be known at compile time").
        if let Some(arg_src) = arg
            && is_boolean_flag_adverb(&name)
            && contains_dynamic_var(arg_src)
        {
            let message = format!("Adverb {} value must be known at compile time", name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Value::Dynamic"), attrs);
            return Err(PError::fatal_with_exception(message, Box::new(ex)));
        }

        // Record the first match-time adverb (those not allowed on rx// or
        // substitution) under its written name, for error reporting.
        if adverbs.first_match_adverb.is_none()
            && matches!(
                name.as_str(),
                "g" | "global"
                    | "ex"
                    | "exhaustive"
                    | "ov"
                    | "overlap"
                    | "p"
                    | "pos"
                    | "c"
                    | "continue"
                    | "nth"
                    | "st"
                    | "nd"
                    | "rd"
                    | "th"
                    | "x"
            )
        {
            adverbs.first_match_adverb = Some(name.clone());
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
            // `:pos(N)` anchors the match to character offset N. A non-literal
            // argument (`:pos($x)`) leaves the value None → falls back to $/.to.
            adverbs.pos_value = arg.and_then(|s| s.trim().parse::<usize>().ok());
        } else if name == "c" || name == "continue" {
            adverbs.continue_ = true;
            adverbs.continue_value = arg.and_then(|s| s.trim().parse::<usize>().ok());
        } else if name.eq_ignore_ascii_case("p5") || name.eq_ignore_ascii_case("perl5") {
            adverbs.perl5 = true;
        } else if matches!(name.as_str(), "nth" | "st" | "nd" | "rd" | "th") {
            // `:st`, `:nd`, `:rd` and `:th` are exact aliases of `:nth` (S05 /
            // `Language/regexes.rakudoc`: "There's actually no difference
            // between the `:nth` adverb and the rest. You choose them only
            // based on legibility"). Both the ordinal-prefix spelling
            // (`:1st`, `:2nd`, `:3rd`, `:4th`) and the parenthesised argument
            // (`:st(1|8)`, `:nth(1,3)`) select the same match ordinal(s).
            if !leading_digits.is_empty() {
                adverbs.nth = Some(leading_digits.clone());
            } else if let Some(raw) = arg {
                adverbs.nth = Some(raw.trim().to_string());
            }
        } else if name == "x" {
            // `:x(N)` (exact count) or `:x(1..3)` (range): keep the raw argument
            // string for the substitution to parse. `:Nx` carries the count in
            // `leading_digits`.
            if let Some(raw) = arg {
                let trimmed = raw.trim();
                if !trimmed.is_empty() {
                    adverbs.repeat = Some(trimmed.to_string());
                }
            } else if !leading_digits.is_empty() {
                adverbs.repeat = Some(leading_digits.clone());
            }
        } else if !leading_digits.is_empty() && name == "x" {
            adverbs.repeat = Some(leading_digits.clone());
        } else {
            return Err(regex_adverb_error(
                &name,
                None,
                format!("Unsupported regex adverb :{}", name),
            ));
        }

        spec = r;
    }
    Ok((spec, adverbs))
}

pub(super) fn parse_compact_match_adverbs<'a>(
    input: &'a str,
    adverbs: &mut MatchAdverbs,
) -> &'a str {
    // The ONLY colonless compact match adverb Raku recognizes is a single
    // `s` directly after `m` (`ms/pattern/`, shorthand for `m:s/pattern/`,
    // per roast/S05-modifier/sigspace.t). Every other letter combination —
    // `mg`, `mi`, `mp5`, `mss`, etc. — is NOT valid Raku (`raku -e 'mi/x/'`
    // dies "Missing required term after infix"); a real routine call or
    // bareword can start with `m` followed by any of those letters (e.g. a
    // `whenever`-bound sigilless `\msg` used as `msg.gist`), so consuming
    // more than this one specific case here mis-parses an ordinary
    // identifier as a bogus regex literal (see
    // news/2026-08/compact-match-adverb-overreach-mis-parses-bareword.md).
    if let Some(rest) = input.strip_prefix('s') {
        adverbs.sigspace = true;
        rest
    } else {
        input
    }
}

pub(super) fn apply_inline_match_adverbs(mut pattern: String, adverbs: &MatchAdverbs) -> String {
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
pub(super) fn adverbs_need_value(adverbs: &MatchAdverbs) -> bool {
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
pub(super) fn build_regex_with_adverbs(pattern: String, adverbs: &MatchAdverbs) -> Value {
    Value::regex_with_adverbs(crate::value::RegexAdverbs {
        pattern: Arc::new(pattern),
        global: adverbs.global,
        exhaustive: adverbs.exhaustive,
        overlap: adverbs.overlap,
        // The `m//` match form keeps `:x` as an exact count; range `:x(1..3)`
        // support lives in the substitution path (parsed from the AST string).
        repeat: adverbs
            .repeat
            .as_ref()
            .and_then(|s| s.parse::<usize>().ok()),
        nth: adverbs.nth.as_ref().map(|s| Arc::new(s.clone())),
        perl5: adverbs.perl5,
        pos: adverbs.pos,
        pos_value: adverbs.pos_value,
        continue_: adverbs.continue_,
        continue_value: adverbs.continue_value,
        ignore_case: adverbs.ignore_case,
        sigspace: adverbs.sigspace,
        samecase: adverbs.samecase,
        samespace: adverbs.samespace,
        // Filled in at *load* time by `OpCode::LoadRegexClosure` when the
        // pattern embeds code; a parsed literal never carries a scope.
        captured: None,
    })
}
