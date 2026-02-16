use super::parse_result::{PError, PResult, parse_char, parse_tag, take_while_opt, take_while1};

use crate::ast::Expr;
use crate::value::Value;

use super::expr::expression;
use super::helpers::ws;

/// Parse an integer literal (including underscore separators).
fn integer(input: &str) -> PResult<'_, Expr> {
    // Hex: 0x...
    if let Ok((rest, _)) = parse_tag(input, "0x") {
        let (rest, digits) = take_while1(rest, |c: char| c.is_ascii_hexdigit() || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 16).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    // Octal: 0o...
    if let Ok((rest, _)) = parse_tag(input, "0o") {
        let (rest, digits) = take_while1(rest, |c: char| matches!(c, '0'..='7' | '_'))?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 8).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    // Binary: 0b...
    if let Ok((rest, _)) = parse_tag(input, "0b") {
        let (rest, digits) = take_while1(rest, |c: char| c == '0' || c == '1' || c == '_')?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 2).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    let (rest, digits) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    // Don't consume if next char is '.' followed by digit (that's a decimal)
    if rest.starts_with('.') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_digit() {
        return Err(PError::expected("integer (not decimal)"));
    }
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    let n: i64 = clean.parse().unwrap_or(0);
    Ok((rest, Expr::Literal(Value::Int(n))))
}

/// Parse a decimal number literal.
fn decimal(input: &str) -> PResult<'_, Expr> {
    let start = input;
    let (rest, _) = take_while1(input, |c: char| c.is_ascii_digit() || c == '_')?;
    let (rest, _) = parse_char(rest, '.')?;
    let (rest, _) = take_while1(rest, |c: char| c.is_ascii_digit() || c == '_')?;
    let num_str = &start[..start.len() - rest.len()];

    // Check for scientific notation
    let (rest, exp_part) = if rest.starts_with('e') || rest.starts_with('E') {
        let exp_start = rest;
        let r = &rest[1..];
        let r = if r.starts_with('+') || r.starts_with('-') {
            &r[1..]
        } else {
            r
        };
        if let Ok((r, _)) = take_while1(r, |c: char| c.is_ascii_digit()) {
            (r, Some(&exp_start[..exp_start.len() - r.len()]))
        } else {
            (rest, None)
        }
    } else {
        (rest, None)
    };

    let full = if let Some(exp) = exp_part {
        format!("{}{}", num_str, exp)
    } else {
        num_str.to_string()
    };
    let clean: String = full.chars().filter(|c| *c != '_').collect();
    let n: f64 = clean.parse().unwrap_or(0.0);
    Ok((rest, Expr::Literal(Value::Num(n))))
}

/// Read a bracketed string with nesting support (e.g., `{...{...}...}`)
fn read_bracketed(input: &str, open: char, close: char) -> PResult<'_, &str> {
    if !input.starts_with(open) {
        return Err(PError::expected(&format!("'{}'", open)));
    }
    let mut rest = &input[open.len_utf8()..];
    let start = rest;
    let mut depth = 1u32;
    loop {
        if rest.is_empty() {
            return Err(PError::expected(&format!("closing '{}'", close)));
        }
        let ch = rest.chars().next().unwrap();
        if ch == '\\' && rest.len() > 1 {
            rest = &rest[2..]; // skip escape
            continue;
        }
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                let content = &start[..start.len() - rest.len()];
                return Ok((&rest[close.len_utf8()..], content));
            }
        }
        rest = &rest[ch.len_utf8()..];
    }
}

/// Parse q{...}, q[...], q(...), q<...>, q/.../ quoting forms.
fn q_string(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('q') {
        return Err(PError::expected("q string"));
    }
    let after_q = &input[1..];
    // Check for qq forms
    let (after_prefix, is_qq) = if let Some(after_qq) = after_q.strip_prefix('q') {
        if after_qq.starts_with('{')
            || after_qq.starts_with('[')
            || after_qq.starts_with('(')
            || after_qq.starts_with('<')
            || after_qq.starts_with('/')
        {
            (after_qq, true)
        } else {
            // Check for q:to heredoc or q with adverbs — not handled here
            (after_q, false)
        }
    } else {
        (after_q, false)
    };
    // Must be followed by a delimiter
    let (open, close) = match after_prefix.chars().next() {
        Some('{') => ('{', '}'),
        Some('[') => ('[', ']'),
        Some('(') => ('(', ')'),
        Some('<') => ('<', '>'),
        Some('/') => {
            // q/.../ — find closing /
            let rest = &after_prefix[1..];
            let end = rest
                .find('/')
                .ok_or_else(|| PError::expected("closing /"))?;
            let content = &rest[..end];
            let rest = &rest[end + 1..];
            let s = if is_qq {
                // qq/.../  — would need interpolation, for now treat as plain
                content.to_string()
            } else {
                content.replace("\\'", "'").replace("\\\\", "\\")
            };
            return Ok((rest, Expr::Literal(Value::Str(s))));
        }
        _ => return Err(PError::expected("q string delimiter")),
    };
    let (rest, content) = read_bracketed(after_prefix, open, close)?;
    let s = if is_qq {
        // qq{...} — would need interpolation, for now treat as plain
        content.to_string()
    } else {
        content.replace("\\'", "'").replace("\\\\", "\\")
    };
    Ok((rest, Expr::Literal(Value::Str(s))))
}

/// Parse a single-quoted string literal.
fn single_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '\'')?;
    let start = input;
    let mut rest = input;
    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing '"));
        }
        if let Some(after_quote) = rest.strip_prefix('\'') {
            let content = &start[..start.len() - rest.len()];
            let s = content.replace("\\'", "'").replace("\\\\", "\\");
            return Ok((after_quote, Expr::Literal(Value::Str(s))));
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            rest = &rest[2..];
        } else {
            let ch = rest.chars().next().unwrap();
            rest = &rest[ch.len_utf8()..];
        }
    }
}

/// Parse a double-quoted string with interpolation support.
fn double_quoted_string(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '"')?;
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(PError::expected("closing \""));
        }
        if rest.starts_with('"') {
            rest = &rest[1..];
            break;
        }
        if rest.starts_with('\\') && rest.len() > 1 {
            let c = rest.as_bytes()[1] as char;
            match c {
                'n' => current.push('\n'),
                't' => current.push('\t'),
                'r' => current.push('\r'),
                '0' => current.push('\0'),
                '\\' => current.push('\\'),
                '"' => current.push('"'),
                '$' => current.push('$'),
                '@' => current.push('@'),
                '{' => current.push('{'),
                'x' => {
                    // \x[HH] or \xHH
                    rest = &rest[2..];
                    if rest.starts_with('[') {
                        if let Some(end) = rest.find(']') {
                            let hex = &rest[1..end];
                            if let Ok(n) = u32::from_str_radix(hex, 16)
                                && let Some(c) = char::from_u32(n)
                            {
                                current.push(c);
                            }
                            rest = &rest[end + 1..];
                        }
                    } else {
                        let hex_chars: String =
                            rest.chars().take_while(|c| c.is_ascii_hexdigit()).collect();
                        let len = hex_chars.len();
                        if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                            && let Some(c) = char::from_u32(n)
                        {
                            current.push(c);
                        }
                        rest = &rest[len..];
                    }
                    continue;
                }
                _ => {
                    current.push('\\');
                    current.push(c);
                }
            }
            rest = &rest[2..];
            continue;
        }
        // Variable interpolation: $var
        if rest.starts_with('$') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' || next == '*' || next == '?' || next == '!' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                // Parse variable name
                let var_rest = &rest[1..];
                let (var_rest, var_name) = parse_var_name_from_str(var_rest);
                parts.push(Expr::Var(var_name));
                rest = var_rest;
                continue;
            }
        }
        // Array interpolation: @var
        if rest.starts_with('@') && rest.len() > 1 {
            let next = rest.as_bytes()[1] as char;
            if next.is_alphabetic() || next == '_' {
                if !current.is_empty() {
                    parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
                }
                let var_rest = &rest[1..];
                let end = var_rest
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(var_rest.len());
                let name = &var_rest[..end];
                parts.push(Expr::ArrayVar(name.to_string()));
                rest = &var_rest[end..];
                continue;
            }
        }
        // Block interpolation: { expr }
        if rest.starts_with('{') {
            if !current.is_empty() {
                parts.push(Expr::Literal(Value::Str(std::mem::take(&mut current))));
            }
            // Find matching close brace (simple, no nesting)
            let mut depth = 0;
            let mut end = 0;
            for (i, c) in rest.char_indices() {
                match c {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            if end > 0 {
                let block_src = &rest[1..end];
                // Parse the block as an expression
                if let Ok((_rest, expr)) = expression(block_src) {
                    parts.push(expr);
                }
                rest = &rest[end + 1..];
                continue;
            }
        }
        let ch = rest.chars().next().unwrap();
        current.push(ch);
        rest = &rest[ch.len_utf8()..];
    }

    if parts.is_empty() {
        Ok((rest, Expr::Literal(Value::Str(current))))
    } else {
        if !current.is_empty() {
            parts.push(Expr::Literal(Value::Str(current)));
        }
        if parts.len() == 1
            && let Expr::Literal(Value::Str(_)) = &parts[0]
        {
            return Ok((rest, parts.into_iter().next().unwrap()));
        }
        Ok((rest, Expr::StringInterpolation(parts)))
    }
}

/// Parse a variable name from raw string (used in interpolation).
fn parse_var_name_from_str(input: &str) -> (&str, String) {
    // Handle twigils: $*, $?, $!
    let (rest, twigil) =
        if input.starts_with('*') || input.starts_with('?') || input.starts_with('!') {
            (&input[1..], &input[..1])
        } else {
            (input, "")
        };
    let end = rest
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
        .unwrap_or(rest.len());
    let name = &rest[..end];
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    (&rest[end..], full_name)
}

/// Parse a $variable reference.
fn scalar_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '$')?;
    // Handle $(expr) — scalar context / itemization
    if input.starts_with('(') {
        return paren_expr(input);
    }
    // Handle $_ special variable
    if input.starts_with('_') && (input.len() == 1 || !input.as_bytes()[1].is_ascii_alphanumeric())
    {
        return Ok((&input[1..], Expr::Var("_".to_string())));
    }
    // Handle $/ (match variable)
    if let Some(stripped) = input.strip_prefix('/') {
        return Ok((stripped, Expr::Var("/".to_string())));
    }
    // Handle twigils: $*FOO, $?FILE, $!attr
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::Var(full_name)))
}

/// Parse an @array variable reference.
fn array_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '@')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
fn hash_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Special: %*ENV
    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Parse a &code variable reference.
fn code_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '&')?;
    // Handle twigils: &?BLOCK, &?ROUTINE
    let (rest, twigil) = if let Some(stripped) = input.strip_prefix('?') {
        (stripped, "?")
    } else {
        (input, "")
    };
    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::CodeVar(full_name)))
}

/// Parse a parenthesized expression or list.
fn paren_expr(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '(')?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_char(input, ')') {
        // Empty parens = empty list
        return Ok((input, Expr::ArrayLiteral(Vec::new())));
    }
    let (input, first) = expression(input)?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, first));
    }
    // Comma-separated list
    let (input, _) = parse_char(input, ',')?;
    let (input, _) = ws(input)?;
    let mut items = vec![first];
    // Handle trailing comma before close paren
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    let (mut input_rest, second) = expression(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        let (input, _) = parse_char(input, ',')?;
        let (input, _) = ws(input)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        let (input, next) = expression(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Parse an array literal [...].
fn array_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '[')?;
    let (input, _) = ws(input)?;
    let mut items = Vec::new();
    if let Ok((input, _)) = parse_char(input, ']') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((r, Expr::ArrayLiteral(items)));
            }
            let (r, next) = expression(r)?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            return Ok((r, Expr::ArrayLiteral(items)));
        }
    }
}

/// Parse a < > quote-word list.
fn angle_list(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '<')?;
    // Make sure it's not <= or <=> etc.
    if input.starts_with('=') || input.starts_with('-') {
        return Err(PError::expected("angle list"));
    }
    let mut words = Vec::new();
    let mut rest = input;
    loop {
        // Skip whitespace
        let (r, _) = take_while_opt(rest, |c: char| c == ' ' || c == '\t');
        rest = r;
        if rest.starts_with('>') {
            rest = &rest[1..];
            break;
        }
        if rest.is_empty() {
            return Err(PError::expected("closing >"));
        }
        let (r, word) = take_while1(rest, |c: char| {
            c != '>' && c != ' ' && c != '\t' && c != '\n'
        })?;
        words.push(word.to_string());
        rest = r;
    }
    if words.len() == 1 {
        Ok((
            rest,
            Expr::Literal(Value::Str(words.into_iter().next().unwrap())),
        ))
    } else {
        let exprs: Vec<Expr> = words
            .into_iter()
            .map(|w| Expr::Literal(Value::Str(w)))
            .collect();
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

/// Parse `Whatever` or `*` as Whatever.
fn whatever(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '*')?;
    // Make sure it's not ** (power op)
    if input.starts_with('*') {
        return Err(PError::expected("whatever (not **)"));
    }
    Ok((input, Expr::Literal(Value::Num(f64::INFINITY))))
}

/// Parse keywords that are values: True, False, Nil, Any, Inf, NaN, etc.
fn keyword_literal(input: &str) -> PResult<'_, Expr> {
    // Try each keyword, ensuring it's not followed by alphanumeric (word boundary)
    let try_kw = |kw: &str, val: Value| -> PResult<'_, Expr> {
        let (rest, _) = parse_tag(input, kw)?;
        // Check word boundary
        if let Some(c) = rest.chars().next()
            && (c.is_alphanumeric() || c == '_' || c == '-')
        {
            return Err(PError::expected("word boundary"));
        }
        Ok((rest, Expr::Literal(val)))
    };

    if let Ok(r) = try_kw("True", Value::Bool(true)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("False", Value::Bool(false)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Nil", Value::Nil) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Any", Value::Nil) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Inf", Value::Num(f64::INFINITY)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("-Inf", Value::Num(f64::NEG_INFINITY)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("NaN", Value::Num(f64::NAN)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("pi", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("tau", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("e", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }
    Err(PError::expected("keyword literal"))
}

/// Parse a bare identifier that could be a type name or function call.
/// Returns Expr::Call for function calls, Expr::BareWord for type names.
/// Check if a name is a Raku keyword (not a function call).
fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "loop"
            | "repeat"
            | "try"
            | "do"
            | "gather"
            | "sub"
            | "my"
            | "our"
            | "has"
            | "class"
            | "role"
            | "module"
            | "use"
            | "need"
            | "import"
            | "require"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "say"
            | "print"
            | "put"
            | "note"
            | "with"
            | "without"
            | "supply"
            | "react"
            | "whenever"
            | "start"
            | "quietly"
    )
}

pub(super) fn identifier_or_call(input: &str) -> PResult<'_, Expr> {
    let (rest, name) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

    // Handle special expression keywords before qualified name resolution
    match name.as_str() {
        "try" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Try { body, catch: None }));
            }
        }
        "do" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::DoBlock { body, label: None }));
            }
            // do if/unless/given/for/while — wrap the control flow statement
            {
                let is_ctrl = |s: &str| {
                    for kw in &["if", "unless", "given", "for", "while", "until"] {
                        if s.starts_with(kw)
                            && !s.as_bytes().get(kw.len()).is_some_and(|&c| {
                                c.is_ascii_alphanumeric() || c == b'_' || c == b'-'
                            })
                        {
                            return true;
                        }
                    }
                    false
                };
                if is_ctrl(r)
                    && let Ok((r, stmt)) = super::stmt::statement_pub(r)
                {
                    return Ok((r, Expr::DoStmt(Box::new(stmt))));
                }
            }
            // do EXPR — just evaluate the expression
            let (r, expr) = expression(r)?;
            return Ok((r, expr));
        }
        "sub" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::AnonSub(body)));
            }
            // sub with params: sub ($x, $y) { ... }
            if r.starts_with('(') {
                // Parse param list, then block
                if let Ok((r2, params_body)) = parse_anon_sub_with_params(r) {
                    return Ok((r2, params_body));
                }
            }
        }
        "gather" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Gather(body)));
            }
        }
        "die" | "fail" => {
            let (r, _) = ws(rest)?;
            // die/fail with no argument
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
                return Ok((r, Expr::Call { name, args: vec![] }));
            }
            let (r, arg) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name,
                    args: vec![arg],
                },
            ));
        }
        "quietly" => {
            let (r, _) = ws(rest)?;
            // quietly expr — wrap in a Call
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: "quietly".to_string(),
                    args: vec![expr],
                },
            ));
        }
        "start" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: "start".to_string(),
                        args: vec![Expr::AnonSub(body)],
                    },
                ));
            }
        }
        "last" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Last,
                    label: None,
                },
            ));
        }
        "next" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Next,
                    label: None,
                },
            ));
        }
        "redo" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Redo,
                    label: None,
                },
            ));
        }
        _ => {}
    }

    // Check for :: qualified name (e.g. Foo::Bar)
    let (rest, name) = {
        let mut full_name = name;
        let mut r = rest;
        while r.starts_with("::") {
            let after = &r[2..];
            if let Ok((rest2, part)) =
                take_while1(after, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            {
                full_name.push_str("::");
                full_name.push_str(part);
                r = rest2;
            } else {
                break;
            }
        }
        (r, full_name)
    };

    // Check if followed by `(` for function call
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, Expr::Call { name, args }));
    }

    // Bareword followed by block { ... } and comma — function call with block arg
    // e.g., map { $_ * 2 }, @arr  or  grep { $_ > 0 }, @arr
    let (r, _) = ws(rest)?;
    if r.starts_with('{')
        && !is_keyword(&name)
        && let Ok((r2, block_body)) = parse_block_body(r)
    {
        let (r3, _) = ws(r2)?;
        if let Some(r3) = r3.strip_prefix(',') {
            // Consume comma and remaining args
            let (r3, _) = ws(r3)?;
            let mut args = vec![Expr::AnonSub(block_body)];
            let (mut r3, first_arg) = expression(r3)?;
            args.push(first_arg);
            loop {
                let (r4, _) = ws(r3)?;
                if !r4.starts_with(',') {
                    return Ok((r4, Expr::Call { name, args }));
                }
                let r4 = &r4[1..];
                let (r4, _) = ws(r4)?;
                if r4.starts_with(';') || r4.is_empty() || r4.starts_with('}') {
                    return Ok((r4, Expr::Call { name, args }));
                }
                let (r4, next_arg) = expression(r4)?;
                args.push(next_arg);
                r3 = r4;
            }
        }
        // Block without trailing comma — return as separate expressions
        // Fall through to BareWord
    }

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
}

/// Parse a block body: { stmts }
fn parse_block_body(input: &str) -> PResult<'_, Vec<crate::ast::Stmt>> {
    let (r, _) = parse_char(input, '{')?;
    let (r, stmts) = super::stmt::stmt_list_pub(r)?;
    let (r, _) = ws_inner(r);
    let (r, _) = parse_char(r, '}')?;
    Ok((r, stmts))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (mut r, _) = parse_char(input, '(')?;
    let mut params = Vec::new();
    loop {
        let (r2, _) = ws(r)?;
        if r2.starts_with(')') {
            r = r2;
            break;
        }
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            let (r3, name) = super::stmt::var_name_pub(r2)?;
            params.push(name);
            let (r3, _) = ws(r3)?;
            if let Some(stripped) = r3.strip_prefix(',') {
                r = stripped;
            } else {
                r = r3;
            }
        } else {
            r = r2;
            break;
        }
    }
    parse_anon_sub_rest(r, params)
}

fn parse_anon_sub_rest(input: &str, params: Vec<String>) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, body) = parse_block_body(r)?;
    if params.is_empty() {
        Ok((r, Expr::AnonSub(body)))
    } else {
        Ok((r, Expr::AnonSubParams { params, body }))
    }
}

/// Parse comma-separated call arguments inside parens.
pub(super) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
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

/// Parse a regex literal: /pattern/ or rx/pattern/ or m/pattern/
fn regex_lit(input: &str) -> PResult<'_, Expr> {
    // rx/pattern/ or rx{pattern}
    if let Ok((rest, _)) = parse_tag(input, "rx") {
        let close_delim = if rest.starts_with('/') {
            '/'
        } else if rest.starts_with('{') {
            '}'
        } else {
            return Err(PError::expected("regex delimiter"));
        };
        let r = &rest[1..];
        let mut end = 0;
        let bytes = r.as_bytes();
        while end < bytes.len() {
            if bytes[end] == close_delim as u8 {
                break;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        let rest = if end < r.len() {
            &r[end + 1..]
        } else {
            &r[end..]
        };
        return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
    }

    // m/pattern/ or m{pattern} or m[pattern]
    if input.starts_with("m/") || input.starts_with("m{") || input.starts_with("m[") {
        let close_delim = match input.as_bytes()[1] {
            b'/' => b'/',
            b'{' => b'}',
            b'[' => b']',
            _ => unreachable!(),
        };
        let r = &input[2..];
        let mut end = 0;
        let bytes = r.as_bytes();
        let mut depth = 1u32;
        while end < bytes.len() {
            if bytes[end] == close_delim {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            if close_delim != b'/' && bytes[end] == input.as_bytes()[1] {
                depth += 1;
            }
            if bytes[end] == b'\\' && end + 1 < bytes.len() {
                end += 2;
            } else {
                end += 1;
            }
        }
        let pattern = &r[..end];
        let rest = if end < r.len() {
            &r[end + 1..]
        } else {
            &r[end..]
        };
        return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
    }

    // Bare /pattern/
    if input.starts_with('/') && !input.starts_with("//") {
        let r = &input[1..];
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
        if end > 0 && end < bytes.len() {
            let pattern = &r[..end];
            let rest = &r[end + 1..];
            return Ok((rest, Expr::Literal(Value::Regex(pattern.to_string()))));
        }
    }

    Err(PError::expected("regex literal"))
}

/// Parse a version literal: v5.26.1
fn version_lit(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, 'v')?;
    // Must start with a digit
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_digit() {
        return Err(PError::expected("version number"));
    }
    let (rest, version) = take_while1(rest, |c: char| {
        c.is_ascii_digit() || c == '.' || c == '*' || c == '+' || c == '-'
    })?;
    let full = format!("v{}", version);
    Ok((rest, Expr::Literal(Value::Str(full))))
}

/// Parse a topicalized method call: .say, .uc, .defined, etc.
fn topic_method_call(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('.') || input.starts_with("..") {
        return Err(PError::expected("topic method call"));
    }
    let r = &input[1..];
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

/// Parse a primary expression (atomic value).
/// Known reduction operators (must be listed to distinguish from array literals).
const REDUCTION_OPS: &[&str] = &[
    "+", "-", "*", "/", "~", "||", "&&", "//", "%%", "**", "+&", "+|", "+^", "?&", "?|", "?^",
    "==", "!=", "<", ">", "<=", ">=", "<=>", "===", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", ",",
];

/// Parse a reduction operator: [+], [*], [~], [min], [max], [gcd], [lcm], [||], [&&], etc.
fn reduction_op(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('[') {
        return Err(PError::expected("reduction operator"));
    }
    let r = &input[1..];
    // Find the closing ]
    let end = r
        .find(']')
        .ok_or_else(|| PError::expected("']' closing reduction"))?;
    let op = &r[..end];
    if op.is_empty() {
        return Err(PError::expected("operator in reduction"));
    }
    // Only accept known operators to avoid confusion with array literals
    if !REDUCTION_OPS.contains(&op) {
        return Err(PError::expected("known reduction operator"));
    }
    let r = &r[end + 1..];
    // Must be followed by whitespace and an expression (not just `]`)
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') || r.starts_with(')') {
        return Err(PError::expected("expression after reduction operator"));
    }
    let (r, _) = ws(r)?;
    // Parse comma-separated list as the operand
    let (r, first) = expression(r)?;
    let mut items = vec![first];
    let mut rest = r;
    loop {
        let (r, _) = ws_inner(rest);
        if !r.starts_with(',') {
            break;
        }
        let r = &r[1..];
        let (r, _) = ws_inner(r);
        // Stop at end-of-input, semicolon, closing brackets, or statement modifiers
        if r.is_empty()
            || r.starts_with(';')
            || r.starts_with('}')
            || r.starts_with(')')
            || r.starts_with(']')
        {
            rest = r;
            break;
        }
        if let Ok((r, next)) = expression(r) {
            items.push(next);
            rest = r;
        } else {
            break;
        }
    }
    let expr = if items.len() == 1 {
        items.remove(0)
    } else {
        Expr::ArrayLiteral(items)
    };
    Ok((
        rest,
        Expr::Reduction {
            op: op.to_string(),
            expr: Box::new(expr),
        },
    ))
}

pub(super) fn primary(input: &str) -> PResult<'_, Expr> {
    if let Ok(r) = decimal(input) {
        return Ok(r);
    }
    if let Ok(r) = integer(input) {
        return Ok(r);
    }
    if let Ok(r) = single_quoted_string(input) {
        return Ok(r);
    }
    if let Ok(r) = double_quoted_string(input) {
        return Ok(r);
    }
    if let Ok(r) = q_string(input) {
        return Ok(r);
    }
    if let Ok(r) = regex_lit(input) {
        return Ok(r);
    }
    if let Ok(r) = version_lit(input) {
        return Ok(r);
    }
    if let Ok(r) = keyword_literal(input) {
        return Ok(r);
    }
    if let Ok(r) = topic_method_call(input) {
        return Ok(r);
    }
    if let Ok(r) = scalar_var(input) {
        return Ok(r);
    }
    if let Ok(r) = array_var(input) {
        return Ok(r);
    }
    if let Ok(r) = hash_var(input) {
        return Ok(r);
    }
    if let Ok(r) = code_var(input) {
        return Ok(r);
    }
    if let Ok(r) = paren_expr(input) {
        return Ok(r);
    }
    if let Ok(r) = reduction_op(input) {
        return Ok(r);
    }
    if let Ok(r) = array_literal(input) {
        return Ok(r);
    }
    if let Ok(r) = angle_list(input) {
        return Ok(r);
    }
    if let Ok(r) = whatever(input) {
        return Ok(r);
    }
    if let Ok(r) = arrow_lambda(input) {
        return Ok(r);
    }
    if let Ok(r) = block_or_hash_expr(input) {
        return Ok(r);
    }
    identifier_or_call(input)
}

/// Parse `-> $param { body }` or `-> $a, $b { body }` arrow lambda.
fn arrow_lambda(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("->") {
        return Err(PError::expected("arrow lambda"));
    }
    let r = &input[2..];
    let (r, _) = ws(r)?;
    // Parse params
    let (r, first) = super::stmt::parse_pointy_param_pub(r)?;
    let (r, _) = ws(r)?;
    if r.starts_with(',') {
        // Multi-param: -> $a, $b { body }
        let mut params = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            let (r2, next) = super::stmt::parse_pointy_param_pub(r2)?;
            params.push(next);
            let (r2, _) = ws(r2)?;
            if !r2.starts_with(',') {
                r = r2;
                break;
            }
            r = r2;
        }
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::AnonSubParams { params, body }))
    } else {
        // Single param: -> $n { body }
        let (r, body) = parse_block_body(r)?;
        Ok((r, Expr::Lambda { param: first, body }))
    }
}

/// Parse a block `{ stmts }` as AnonSub or `{}` / `{ key => val, ... }` as Hash.
fn block_or_hash_expr(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with('{') {
        return Err(PError::expected("block or hash"));
    }
    let r = &input[1..];
    let (r, _) = ws_inner(r);

    // Empty hash: {}
    if let Some(rest) = r.strip_prefix('}') {
        return Ok((rest, Expr::Hash(Vec::new())));
    }

    // Try to detect if this is a hash literal: { key => val, ... }
    // Heuristic: if after ws we see `ident =>` or `"str" =>` or `'str' =>`, it's a hash
    if is_hash_literal_start(r) {
        return parse_hash_literal_body(r);
    }

    // Otherwise parse as a block (anonymous sub)
    let (r, stmts) = super::stmt::stmt_list_pub(r)?;
    let (r, _) = ws_inner(r);
    if !r.starts_with('}') {
        return Err(PError::expected("'}'"));
    }
    let r = &r[1..];
    Ok((r, Expr::AnonSub(stmts)))
}

/// Simple whitespace consumer that doesn't use PResult (infallible).
fn ws_inner(input: &str) -> (&str, ()) {
    match super::helpers::ws(input) {
        Ok((r, _)) => (r, ()),
        Err(_) => (input, ()),
    }
}

/// Check if the input looks like a hash literal start.
fn is_hash_literal_start(input: &str) -> bool {
    // ident => or "str" => or 'str' =>
    if let Ok((r, _)) = super::stmt::ident_pub(input) {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    // Quoted key => val
    if (input.starts_with('"') || input.starts_with('\''))
        && let Ok((r, _)) = single_quoted_string(input).or_else(|_| double_quoted_string(input))
    {
        let (r, _) = ws_inner(r);
        if r.starts_with("=>") {
            return true;
        }
    }
    false
}

/// Parse hash literal body: key => val, key => val, ... }
fn parse_hash_literal_body(input: &str) -> PResult<'_, Expr> {
    let mut pairs = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws_inner(rest);
        if let Some(rest) = r.strip_prefix('}') {
            return Ok((rest, Expr::Hash(pairs)));
        }
        // Parse key as identifier or string
        let (r, key) = if let Ok((r, name)) = super::stmt::ident_pub(r) {
            (r, name)
        } else if let Ok((r, Expr::Literal(Value::Str(s)))) =
            single_quoted_string(r).or_else(|_| double_quoted_string(r))
        {
            (r, s)
        } else {
            return Err(PError::expected("hash key"));
        };
        let (r, _) = ws_inner(r);
        // Expect =>
        if !r.starts_with("=>") {
            return Err(PError::expected("'=>' in hash literal"));
        }
        let r = &r[2..];
        let (r, _) = ws_inner(r);
        let (r, val) = super::expr::expression(r)?;
        pairs.push((key, Some(val)));
        let (r, _) = ws_inner(r);
        if let Some(stripped) = r.strip_prefix(',') {
            rest = stripped;
        } else {
            rest = r;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_integer() {
        let (rest, expr) = primary("42").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(42))));
    }

    #[test]
    fn parse_hex() {
        let (rest, expr) = primary("0xFF").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Int(255))));
    }

    #[test]
    fn parse_scalar() {
        let (rest, expr) = primary("$x").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "x"));
    }

    #[test]
    fn parse_twigil_var() {
        let (rest, expr) = primary("$*OUT").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "*OUT"));
    }

    #[test]
    fn parse_angle_single() {
        let (rest, expr) = primary("<hello>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Literal(Value::Str(ref s)) if s == "hello"));
    }

    #[test]
    fn parse_angle_list() {
        let (rest, expr) = primary("<a b c>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::ArrayLiteral(ref items) if items.len() == 3));
    }

    #[test]
    fn parse_dq_interpolation() {
        let (rest, expr) = primary("\"hello $x world\"").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::StringInterpolation(_)));
    }
}
