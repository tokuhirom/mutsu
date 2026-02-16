use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while, take_while1};
use nom::character::complete::char;
use nom::combinator::{opt, recognize};
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair};

use crate::ast::Expr;
use crate::value::Value;

use super::expr::expression;
use super::helpers::ws;

/// Parse an integer literal (including underscore separators).
fn integer(input: &str) -> IResult<&str, Expr> {
    // Hex: 0x...
    if let Ok((rest, _)) = tag::<&str, &str, nom::error::Error<&str>>("0x")(input) {
        let (rest, digits) = take_while1(|c: char| c.is_ascii_hexdigit() || c == '_')(rest)?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 16).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    // Octal: 0o...
    if let Ok((rest, _)) = tag::<&str, &str, nom::error::Error<&str>>("0o")(input) {
        let (rest, digits) = take_while1(|c: char| matches!(c, '0'..='7' | '_'))(rest)?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 8).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    // Binary: 0b...
    if let Ok((rest, _)) = tag::<&str, &str, nom::error::Error<&str>>("0b")(input) {
        let (rest, digits) = take_while1(|c: char| c == '0' || c == '1' || c == '_')(rest)?;
        let clean: String = digits.chars().filter(|c| *c != '_').collect();
        let n = i64::from_str_radix(&clean, 2).unwrap_or(0);
        return Ok((rest, Expr::Literal(Value::Int(n))));
    }
    let (rest, digits) = take_while1(|c: char| c.is_ascii_digit() || c == '_')(input)?;
    // Don't consume if next char is '.' followed by digit (that's a decimal)
    if rest.starts_with('.') && rest.len() > 1 && rest.as_bytes()[1].is_ascii_digit() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let clean: String = digits.chars().filter(|c| *c != '_').collect();
    let n: i64 = clean.parse().unwrap_or(0);
    Ok((rest, Expr::Literal(Value::Int(n))))
}

/// Parse a decimal number literal.
fn decimal(input: &str) -> IResult<&str, Expr> {
    let (rest, num_str) = recognize(pair(
        take_while1(|c: char| c.is_ascii_digit() || c == '_'),
        pair(
            char('.'),
            take_while1(|c: char| c.is_ascii_digit() || c == '_'),
        ),
    ))(input)?;
    // Check for scientific notation
    let (rest, exp_part) = opt(recognize(pair(
        alt((char('e'), char('E'))),
        recognize(pair(
            opt(alt((char('+'), char('-')))),
            take_while1(|c: char| c.is_ascii_digit()),
        )),
    )))(rest)?;
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
fn single_quoted_string(input: &str) -> IResult<&str, Expr> {
    let (input, content) = delimited(
        char('\''),
        recognize(nom::multi::many0(alt((
            recognize(pair(char('\\'), nom::character::complete::anychar)),
            recognize(take_while1(|c: char| c != '\'' && c != '\\')),
        )))),
        char('\''),
    )(input)?;
    let s = content.replace("\\'", "'").replace("\\\\", "\\");
    Ok((input, Expr::Literal(Value::Str(s))))
}

/// Parse a double-quoted string with interpolation support.
fn double_quoted_string(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('"')(input)?;
    let mut parts: Vec<Expr> = Vec::new();
    let mut current = String::new();
    let mut rest = input;

    loop {
        if rest.is_empty() {
            return Err(nom::Err::Error(nom::error::Error::new(
                rest,
                nom::error::ErrorKind::Tag,
            )));
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
fn scalar_var(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('$')(input)?;
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
    let (rest, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::Var(full_name)))
}

/// Parse an @array variable reference.
fn array_var(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('@')(input)?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
fn hash_var(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('%')(input)?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Special: %*ENV
    let (rest, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(rest)?;
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Parse a &code variable reference.
fn code_var(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('&')(input)?;
    let (rest, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)?;
    Ok((rest, Expr::CodeVar(name.to_string())))
}

/// Parse a parenthesized expression or list.
fn paren_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = char::<&str, nom::error::Error<&str>>(')')(input) {
        // Empty parens = empty list
        return Ok((input, Expr::ArrayLiteral(Vec::new())));
    }
    let (input, first) = expression(input)?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = char::<&str, nom::error::Error<&str>>(')')(input) {
        return Ok((input, first));
    }
    // Comma-separated list
    let (input, _) = char(',')(input)?;
    let (input, _) = ws(input)?;
    let mut items = vec![first];
    // Handle trailing comma before close paren
    if let Ok((input, _)) = char::<&str, nom::error::Error<&str>>(')')(input) {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    let (mut input_rest, second) = expression(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
        if let Ok((input, _)) = char::<&str, nom::error::Error<&str>>(')')(input) {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        let (input, _) = char(',')(input)?;
        let (input, _) = ws(input)?;
        if let Ok((input, _)) = char::<&str, nom::error::Error<&str>>(')')(input) {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        let (input, next) = expression(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Parse an array literal [...].
fn array_literal(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('[')(input)?;
    let (input, _) = ws(input)?;
    let (input, items) = separated_list0(delimited(ws, char(','), ws), expression)(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = opt(char(','))(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::ArrayLiteral(items)))
}

/// Parse a < > quote-word list.
fn angle_list(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('<')(input)?;
    // Make sure it's not <= or <=> etc.
    if input.starts_with('=') || input.starts_with('-') {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    let mut words = Vec::new();
    let mut rest = input;
    loop {
        // Skip whitespace
        let (r, _) = take_while(|c: char| c == ' ' || c == '\t')(rest)?;
        rest = r;
        if rest.starts_with('>') {
            rest = &rest[1..];
            break;
        }
        if rest.is_empty() {
            return Err(nom::Err::Error(nom::error::Error::new(
                rest,
                nom::error::ErrorKind::Tag,
            )));
        }
        let (r, word) =
            take_while1(|c: char| c != '>' && c != ' ' && c != '\t' && c != '\n')(rest)?;
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
fn whatever(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('*')(input)?;
    // Make sure it's not ** (power op)
    if input.starts_with('*') {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    Ok((input, Expr::Literal(Value::Num(f64::INFINITY))))
}

/// Parse keywords that are values: True, False, Nil, Any, Inf, NaN, etc.
fn keyword_literal(input: &str) -> IResult<&str, Expr> {
    // Try each keyword, ensuring it's not followed by alphanumeric (word boundary)
    let try_kw = |kw: &str, val: Value| -> IResult<&str, Expr> {
        let (rest, _) = tag(kw)(input)?;
        // Check word boundary
        if let Some(c) = rest.chars().next()
            && (c.is_alphanumeric() || c == '_' || c == '-')
        {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )));
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
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

/// Parse a bare identifier that could be a type name or function call.
/// Returns Expr::Call for function calls, Expr::BareWord for type names.
pub(super) fn identifier_or_call(input: &str) -> IResult<&str, Expr> {
    let (rest, name) = take_while1(|c: char| c.is_alphanumeric() || c == '_' || c == '-')(input)?;
    let name = name.to_string();

    // Check for :: qualified name (e.g. Foo::Bar)
    let (rest, name) = {
        let mut full_name = name;
        let mut r = rest;
        while r.starts_with("::") {
            let after = &r[2..];
            if let Ok((rest2, part)) = take_while1::<_, &str, nom::error::Error<&str>>(|c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })(after)
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
        let (rest, _) = char('(')(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = char(')')(rest)?;
        return Ok((rest, Expr::Call { name, args }));
    }

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
}

/// Parse comma-separated call arguments inside parens.
pub(super) fn parse_call_arg_list(input: &str) -> IResult<&str, Vec<Expr>> {
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
        let (r, _) = char(',')(r)?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, args));
        }
        let (r, arg) = expression(r)?;
        args.push(arg);
        rest = r;
    }
}

/// Parse a `regex` / `rx` literal: rx/ ... / or / ... /
#[allow(dead_code)]
fn regex_literal(input: &str) -> IResult<&str, Expr> {
    let (input, _) = alt((tag("rx/"), tag("rx!")))(input)?;
    let delim = if !input.is_empty() && &input[..0] == "!" {
        '!'
    } else {
        '/'
    };
    // Actually, we consumed rx/ or rx!, so the delimiter was already consumed
    // We need to find the closing delimiter
    let close = if delim == '!' { '!' } else { '/' };
    // Hmm, let me redo: tag("rx/") consumes "rx/", so closing is "/"
    // tag("rx!") consumes "rx!", so closing is "!"
    let _ = close;
    // Find the matching close delimiter (simple, no escaping for now)
    // Actually the delimiter was part of the tag, so:
    let input = if let Some(stripped) = input.strip_prefix('/') {
        stripped
    } else {
        input
    };
    // The opening delimiter was already consumed as part of tag("rx/")
    // so we need to scan for the closing /
    // Wait, tag("rx/") matches "rx/" exactly, so after this, rest starts with the pattern
    // Actually I need to reconsider. tag("rx/") means "rx" followed by "/".
    // So after consuming, `input` is the regex body. We need to find the next unescaped /
    let mut end = 0;
    let bytes = input.as_bytes();
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
    let pattern = &input[..end];
    let rest = if end < input.len() {
        &input[end + 1..]
    } else {
        &input[end..]
    };
    Ok((rest, Expr::Literal(Value::Str(pattern.to_string()))))
}

/// Parse a primary expression (atomic value).
pub(super) fn primary(input: &str) -> IResult<&str, Expr> {
    alt((
        decimal,
        integer,
        single_quoted_string,
        double_quoted_string,
        keyword_literal,
        scalar_var,
        array_var,
        hash_var,
        code_var,
        paren_expr,
        array_literal,
        angle_list,
        whatever,
        identifier_or_call,
    ))(input)
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
