use super::*;

// Find the enclosing regex delimiter while respecting regex syntax and embedded code.
pub(super) fn scan_to_delim_inner(
    input: &str,
    open_ch: char,
    close_ch: char,
    is_paired: bool,
    subst_pattern: bool,
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
        } else if c == '#' {
            // # starts a comment in Raku regex.
            // #`[...] is an embedded comment (bracket-delimited), and so are
            // the declarator blocks #|{...} / #={...}: the regex's whitespace
            // is the main language's, which may span lines.
            // Plain # is a line comment (until end of line).
            if let Some(rest) = crate::parser::helpers::skip_bracketed_comment(&input[i..]) {
                let end = input.len() - rest.len();
                while chars.clone().next().is_some_and(|(j, _)| j < end) {
                    chars.next();
                }
            } else if let Some((_, '`')) = chars.clone().next() {
                chars.next(); // skip `
                if let Some((_, bracket)) = chars.next() {
                    // `#`«...»`, `#`[...]`, etc. — any bracket pair Raku accepts.
                    // Falls back to the same char for a non-bracket delimiter.
                    let close =
                        crate::parser::helpers::matching_bracket(bracket).unwrap_or(bracket);
                    let mut embed_depth = 1u32;
                    for (_, ch) in chars.by_ref() {
                        if ch == bracket && bracket != close {
                            embed_depth += 1;
                        } else if ch == close {
                            embed_depth -= 1;
                            if embed_depth == 0 {
                                break;
                            }
                        }
                    }
                }
            } else {
                for (_, ch) in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
            }
        } else if c == '<' && input[i + 1..].starts_with('<') {
            // << is a left word boundary assertion — skip both chars
            chars.next(); // consume second <
        } else if c == '>' && input[i + 1..].starts_with('>') {
            // >> is a right word boundary assertion — skip both chars
            chars.next(); // consume second >
        } else if c == '{' {
            // A bare `{ ... }` embedded code block is Main-slang code, not regex:
            // skip the whole balanced (string-aware) brace block so a delimiter
            // inside it — most commonly the `/` of the `$/` match variable
            // (`/ (\d) { say $/ } \d+ /`) — does not end the regex early.
            skip_interp_block(&mut chars)?;
        } else if c == ':' && starts_regex_decl(&input[i + 1..]) {
            // `:my $c = $/;` / `:our …` / `:constant …` / `:let …` / `:temp …`
            // (scalar form) or `:my token NAME { … }` (block form, a
            // lexically-scoped named sub-rule) — an embedded declaration
            // whose body is Main-slang code, not regex text. Skip the whole
            // clause (see `skip_regex_decl_clause`) so a `/` inside it —
            // typically the `/` of `$/` — is not mistaken for the enclosing
            // regex's own closing delimiter.
            skip_regex_decl_clause(&mut chars, &input[i + 1..])?;
        } else if c == '<' && starts_char_class(&input[i + 1..]) {
            // Skip character class <[...]>, <-[...]>, <+[...]>, <![...]> content
            // without interpreting quotes. Handles <['"]>, <-["\\\t]>, etc.
            skip_char_class(&mut chars)?;
        } else if c == '<' && !input[i + 1..].starts_with('[') && !input[i + 1..].starts_with('(') {
            // Track angle bracket nesting for regex constructs.
            // Prevents # inside <...> from being treated as a comment,
            // and { } inside <?{...}> from affecting brace depth.
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
                // The assertion body is Raku code, so a brace in a quoted
                // string is not its closing brace (`<?{ $x eq '}' }>`).
                // Reuse the quote-aware scanner used by ordinary embedded
                // code blocks rather than counting braces in raw source.
                skip_interp_block(&mut chars)?;
                // Consume the closing >
                if let Some((_, '>')) = chars.next() {
                    // done
                }
            } else {
                // Named assertions, Unicode props, etc.: track <> depth
                // Also track (...) so that > inside parens (e.g. => in args)
                // doesn't prematurely close the angle brackets.
                let mut angle_depth = 1u32;
                let mut paren_depth = 0u32;
                // Inside a LOOKAROUND the body is a regex, so a quoted literal
                // there may contain the angle brackets themselves —
                // `<!before '%>' >` and `<!before '<%' >` are both ordinary
                // Raku, and their quoted content must not move the angle depth.
                // Only lookarounds get this treatment: in a word-list
                // alternation (`< a ' b >`) or a character class a quote
                // character is just a literal, and skipping to a "terminator"
                // that never comes would swallow the rest of the regex.
                let honor_quotes = [
                    "before ", "?before ", "!before ", ".before ", "after ", "?after ", "!after ",
                    ".after ",
                ]
                .iter()
                .any(|kw| remaining.starts_with(kw));
                loop {
                    match chars.next() {
                        Some((_, q)) if honor_quotes && is_regex_quote_open(q) => loop {
                            match chars.next() {
                                // `｢...｣` is raw (Q): a backslash is literal.
                                Some((_, '\\')) if q != '\u{FF62}' => {
                                    chars.next();
                                }
                                Some((_, ch)) if is_regex_quote_terminator(q, ch) => break,
                                Some(_) => {}
                                None => return None,
                            }
                        },
                        // A nested character class inside the assertion
                        // (`<!before '"' <-["]>*? >`): its content is literal,
                        // so a quote or bracket char in it must not move the
                        // quote/angle state. Without this, the class's `"`
                        // opens a bogus quote (when honor_quotes) that
                        // swallows the rest of the regex.
                        Some((i2, '<'))
                            if paren_depth == 0 && starts_char_class(&input[i2 + 1..]) =>
                        {
                            skip_char_class(&mut chars)?;
                        }
                        Some((_, '(')) => paren_depth += 1,
                        Some((_, ')')) => paren_depth = paren_depth.saturating_sub(1),
                        Some((_, '<')) if paren_depth == 0 => angle_depth += 1,
                        Some((_, '>')) if paren_depth == 0 => {
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
        } else if is_regex_quote_open(c) {
            // Skip quoted string content in regex (e.g., '/' or '\\').
            // This prevents delimiters inside string atoms like m/ "/" ** 2 /
            // from prematurely ending the regex literal.
            loop {
                match chars.next() {
                    // `｢...｣` is raw (Q): a backslash is literal.
                    Some((_, '\\')) if c != '\u{FF62}' => {
                        chars.next(); // skip escaped char
                    }
                    Some((_, ch)) if is_regex_quote_terminator(c, ch) => break,
                    Some(_) => {}
                    None => return None,
                }
            }
        } else if (c == '@' || c == '$') && input[i + c.len_utf8()..].starts_with('(') {
            // The contextualizer body is Main-slang code. Its delimiter and
            // quoted parentheses cannot close the surrounding regex.
            chars.next(); // skip '('
            let mut paren_depth = 1u32;
            let mut prev_sig = '(';
            loop {
                match chars.next() {
                    Some((_, 'r'))
                        if crate::regex_code_nested::slash_opens_regex_after(prev_sig)
                            && chars.as_str().starts_with("x/") =>
                    {
                        chars.next(); // skip 'x'
                        chars.next(); // skip '/'
                        skip_nested_slash_regex(&mut chars)?;
                        prev_sig = '/';
                    }
                    Some((_, '(')) => {
                        paren_depth += 1;
                        prev_sig = '(';
                    }
                    Some((_, ')')) => {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            break;
                        }
                        prev_sig = ')';
                    }
                    Some((_, quote @ ('\'' | '"'))) => loop {
                        match chars.next() {
                            Some((_, '\\')) => {
                                chars.next();
                            }
                            Some((_, ch)) if ch == quote => break,
                            Some(_) => {}
                            None => return None,
                        }
                    },
                    Some((_, '\\')) => {
                        chars.next();
                    }
                    Some((_, ch)) => {
                        if !ch.is_whitespace() {
                            prev_sig = ch;
                        }
                    }
                    None => return None,
                }
            }
        } else if !subst_pattern && c == '$' && !is_paired {
            // In non-paired delimiters (like /), $ followed by the close
            // delimiter MIGHT be a variable reference ($/ is the match variable)
            // or it might be the end-of-string anchor followed by the closing
            // delimiter. Disambiguate: if $/ is followed by [ or . or < it's
            // the variable; otherwise it's anchor + close. Skipped for a
            // substitution pattern, where the delimiter always separates and a
            // trailing `$` is unambiguously the anchor (`s/foo$/.bar/`).
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
