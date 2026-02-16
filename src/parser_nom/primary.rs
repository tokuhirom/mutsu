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
    let (rest, name) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    Ok((rest, Expr::CodeVar(name.to_string())))
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
pub(super) fn identifier_or_call(input: &str) -> PResult<'_, Expr> {
    let (rest, name) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

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

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
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
        return Ok((rest, Expr::Literal(Value::Str(pattern.to_string()))));
    }

    // m/pattern/
    if let Some(stripped) = input.strip_prefix("m/") {
        let r = stripped;
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
        let rest = if end < r.len() {
            &r[end + 1..]
        } else {
            &r[end..]
        };
        return Ok((rest, Expr::Literal(Value::Str(pattern.to_string()))));
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
            return Ok((rest, Expr::Literal(Value::Str(pattern.to_string()))));
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
        c.is_ascii_digit() || c == '.' || c == '*' || c == '+'
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
    if let Ok(r) = array_literal(input) {
        return Ok(r);
    }
    if let Ok(r) = angle_list(input) {
        return Ok(r);
    }
    if let Ok(r) = whatever(input) {
        return Ok(r);
    }
    identifier_or_call(input)
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
