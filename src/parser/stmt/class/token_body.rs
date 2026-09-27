use super::rule_ws::inject_implicit_rule_ws;
use crate::ast::Stmt;
use crate::parser::parse_result::{PError, PResult, take_while1};
use crate::parser::primary::regex::scan_to_delim;
use crate::parser::stmt::ident;

pub(crate) fn consume_raw_braced_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("raw braced body"));
    }
    let mut depth = 0u32;
    let mut i = 0usize;
    while i < input.len() {
        let ch = input[i..]
            .chars()
            .next()
            .ok_or_else(|| PError::expected("closing '}'"))?;
        let len = ch.len_utf8();
        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    let rest = &input[i + len..];
                    return Ok((rest, Vec::new()));
                }
            }
            '\\' => {
                i += len;
                if i < input.len() {
                    let next_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(0);
                    i += next_len;
                    continue;
                }
            }
            '\'' | '"' => {
                let quote = ch;
                i += len;
                while i < input.len() {
                    let c = input[i..]
                        .chars()
                        .next()
                        .ok_or_else(|| PError::expected("string close"))?;
                    let c_len = c.len_utf8();
                    if c == '\\' {
                        i += c_len;
                        if i < input.len() {
                            let n_len =
                                input[i..].chars().next().map(|n| n.len_utf8()).unwrap_or(0);
                            i += n_len;
                            continue;
                        }
                    }
                    i += c_len;
                    if c == quote {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += len;
    }
    Err(PError::expected("closing '}'"))
}

/// The content of a `:sym<...>` (or `«...»` / `<<...>>`) adverb is word-quoted,
/// so surrounding whitespace is insignificant: `:sym<foo >` names the same
/// candidate as `:sym<foo>`. Trim it so a grammar token candidate whose sym
/// carries stray whitespace still matches its action method's `:sym<foo>`
/// (e.g. POFile's `token comment:sym<format-directive >` vs
/// `method comment:sym<format-directive>`). A whitespace-only symbol is left
/// verbatim so null-operator detection (`infix:< >`) still sees the spacing.
pub(crate) fn sym_adverb_inner(inner: &str) -> &str {
    let trimmed = inner.trim();
    if trimmed.is_empty() { inner } else { trimmed }
}

pub(crate) fn parse_token_like_name(input: &str) -> PResult<'_, String> {
    let (mut rest, mut name) = ident(input)?;
    loop {
        // `::`-qualified name segments (e.g. `does DBDish::ErrorHandling`).
        // Handle these before the single-`:` adverb logic so a qualified role
        // name parses as one token rather than failing on the second segment.
        if rest.starts_with("::") && !rest[2..].starts_with('(') {
            if let Ok((r, seg)) = ident(&rest[2..]) {
                name.push_str("::");
                name.push_str(&seg);
                rest = r;
                continue;
            }
            break;
        }
        if !rest.starts_with(':') {
            break;
        }
        let r = &rest[1..];
        // `:sym<value>` has an identifier part; `:<value>` (shorthand for
        // `:sym<value>`) has the colon immediately followed by `<...>` with no
        // part name. Allow an empty part in that case.
        let (r, part) = if r.starts_with('<') || r.starts_with("<<") || r.starts_with('\u{ab}') {
            (r, "")
        } else {
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?
        };
        name.push(':');
        name.push_str(part);
        let mut r2 = r;
        if r2.starts_with("<<") {
            // <<...>> delimiter (ASCII double-angle quotes)
            let after_open = &r2[2..];
            if let Some(end) = after_open.find(">>") {
                // Store as «...» internally for consistency
                name.push('\u{ab}');
                name.push_str(sym_adverb_inner(&after_open[..end]));
                name.push('\u{bb}');
                r2 = &after_open[end + 2..];
            }
        } else if r2.starts_with('<')
            && let Some(end) = r2.find('>')
        {
            name.push('<');
            name.push_str(sym_adverb_inner(&r2[1..end]));
            name.push('>');
            r2 = &r2[end + 1..];
        } else if r2.starts_with('\u{ab}') {
            // «» (French quotes) — keep as «» internally to avoid
            // ambiguity when the value contains '>'
            let after_open = &r2['\u{ab}'.len_utf8()..];
            if let Some(end) = after_open.find('\u{bb}') {
                name.push('\u{ab}');
                name.push_str(sym_adverb_inner(&after_open[..end]));
                name.push('\u{bb}');
                r2 = &after_open[end + '\u{bb}'.len_utf8()..];
            }
        }
        rest = r2;
    }
    Ok((rest, name))
}

pub(crate) fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) = scan_to_delim(after_open, '{', '}', true) {
        // Trailing whitespace is preserved (only leading is dropped): a `rule`
        // inserts an implicit `<.ws>` right up to the closing `}`
        // (`rule_ws::inject_implicit_rule_ws`), so trimming it away here would
        // starve that pass of the whitespace run it needs to see. A plain
        // `token`/`regex` (non-`rule`) is unaffected either way — its
        // tokenizer treats inter-atom pattern whitespace as pure layout and
        // silently skips a trailing run with no atom after it.
        return Ok((rest, body.trim_start().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}

/// In a `rule`, the `%` separator quantifier should allow optional whitespace
/// around the separator. This function finds `% SEPARATOR` patterns and wraps
/// the separator with `[ <.ws>? SEPARATOR <.ws>? ]`.
pub(crate) fn inject_separator_ws(pattern: &str) -> String {
    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut escaped = false;
    let mut in_single = false;
    let mut in_double = false;
    let mut brace_depth = 0usize;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        // A `{ … }` code block (and the body of a `<?{ … }>` assertion) is
        // main-slang code, so a `%hash` in it is a variable, not the `%`
        // separator quantifier. Copy it through verbatim — mangling it into
        // `%[ <.ws>? h <.ws>? ]ash` silently corrupted the block.
        if !in_single && !in_double && (c == '{' || brace_depth > 0) {
            if c == '{' {
                brace_depth += 1;
            } else if c == '}' {
                brace_depth -= 1;
            }
            out.push(c);
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        if in_single || in_double {
            out.push(c);
            i += 1;
            continue;
        }
        // An embedded declaration `:my … ;` is main-slang code; a `%*var` in it
        // must not be treated as a `%` separator. Copy it through verbatim.
        if c == ':' {
            let rest: String = chars[i + 1..].iter().collect();
            if rest.starts_with("my ")
                || rest.starts_with("our ")
                || rest.starts_with("constant ")
                || rest.starts_with("let ")
                || rest.starts_with("temp ")
            {
                while i < chars.len() {
                    let ch = chars[i];
                    out.push(ch);
                    i += 1;
                    if ch == ';' {
                        break;
                    }
                }
                continue;
            }
        }
        // Look for `%` (or `%%`) followed by a separator expression
        if c == '%' {
            out.push('%');
            i += 1;
            // `%%` (trailing-separator-allowed) is a single operator: emit its
            // second `%` too, otherwise the second `%` is mistaken for the
            // separator atom and the operator is destroyed (e.g.
            // `\d+ %% "+"` would become `\d+ % [<.ws>? % <.ws>?] "+"`).
            if i < chars.len() && chars[i] == '%' {
                out.push('%');
                i += 1;
            }
            // Skip whitespace after % / %%
            while i < chars.len() && chars[i].is_whitespace() {
                out.push(chars[i]);
                i += 1;
            }
            if i >= chars.len() {
                continue;
            }
            // Extract the separator expression
            let sep_start = i;
            if chars[i] == '[' {
                // Bracketed separator: % [ ... ]
                // Scan to matching ]
                let mut depth = 1usize;
                i += 1;
                while i < chars.len() && depth > 0 {
                    if chars[i] == '[' {
                        depth += 1;
                    } else if chars[i] == ']' {
                        depth -= 1;
                    } else if chars[i] == '\\' {
                        i += 1; // skip escaped char
                    }
                    i += 1;
                }
                let sep_end = i;
                let optional = chars.get(i) == Some(&'?');
                if optional {
                    i += 1;
                }
                let sep: String = chars[sep_start..sep_end].iter().collect();
                if optional {
                    out.push_str(&format!("[ <.ws>? {}? <.ws>? ]", sep.trim()));
                } else {
                    out.push_str(&format!("[ <.ws>? {} <.ws>? ]", sep.trim()));
                }
            } else {
                // Non-bracketed separator: % \, or % ","
                // Collect the separator atom (could be \X, 'str', "str", or a single char)
                let atom_start = i;
                if chars[i] == '\'' {
                    // Single-quoted string
                    i += 1;
                    while i < chars.len() && chars[i] != '\'' {
                        if chars[i] == '\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    if i < chars.len() {
                        i += 1;
                    } // skip closing '
                } else if chars[i] == '"' {
                    // Double-quoted string
                    i += 1;
                    while i < chars.len() && chars[i] != '"' {
                        if chars[i] == '\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    if i < chars.len() {
                        i += 1;
                    } // skip closing "
                } else if chars[i] == '\\' {
                    // Backslash-escaped char
                    i += 2;
                } else if chars[i] == '<' {
                    // Angle-bracket assertion
                    let mut depth = 1usize;
                    i += 1;
                    while i < chars.len() && depth > 0 {
                        if chars[i] == '<' {
                            depth += 1;
                        } else if chars[i] == '>' {
                            depth -= 1;
                        }
                        i += 1;
                    }
                } else {
                    // Single character separator
                    i += 1;
                }
                let sep_end = i;
                let optional = chars.get(i) == Some(&'?');
                if optional {
                    i += 1;
                }
                let sep: String = chars[atom_start..sep_end].iter().collect();
                if optional {
                    out.push_str(&format!("[ <.ws>? {}? <.ws>? ]", sep.trim()));
                } else {
                    out.push_str(&format!("[ <.ws>? {} <.ws>? ]", sep.trim()));
                }
            }
            continue;
        }
        out.push(c);
        i += 1;
    }
    out
}

pub(crate) fn normalize_token_pattern(pattern: &str) -> String {
    let trimmed = pattern.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        // Trailing whitespace is preserved here too (see
        // `parse_raw_braced_regex_body`) — only the `/…/`-wrapped form above
        // needs the fully-trimmed view, to detect and strip the delimiters.
        pattern.trim_start().to_string()
    }
}

/// Turn an already-[`normalize_token_pattern`]d `token`/`regex`/`rule` body
/// into the pattern the engine executes: `rule`-flavoured whitespace injected
/// and the ratchet prefix added for the two ratcheting declarators.
///
/// This is the *anonymous* declarator's finalizer (`token ($x) { … }` in
/// `primary::ident::identifier_call`), which previously built only the
/// normalized text — so an anonymous `rule` got none of its implicit `<.ws>`
/// and an anonymous `token` did not ratchet. The named declarator
/// (`grammar_module::token_decl`) does the same steps inline. A `:sym<…>`
/// candidate gets no extra trailing `<.ws>` there either: like any `rule`, it
/// ends in `<.ws>` only when its body has whitespace before the closing `}`
/// (mutsu#9094).
pub(crate) fn finalize_anon_declarator_pattern(
    normalized: &str,
    kind: crate::regex_tree::RegexDeclKind,
) -> String {
    use crate::regex_tree::RegexDeclKind;
    let mut pattern = normalized.to_string();
    if kind == RegexDeclKind::Rule {
        pattern = inject_implicit_rule_ws(&pattern);
        pattern = inject_separator_ws(&pattern);
    }
    if kind != RegexDeclKind::Regex {
        pattern = format!(":ratchet {pattern}");
    }
    pattern
}
