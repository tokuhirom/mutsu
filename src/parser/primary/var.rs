use super::super::parse_result::{PError, PResult, parse_char, take_while1};

use crate::ast::Expr;
use crate::value::Value;

use super::super::expr::expression;
use super::super::helpers::ws;
use super::container::paren_expr;
use super::current_line_number;

/// Parse a variable name from raw string (used in interpolation).
pub(super) fn parse_var_name_from_str(input: &str) -> (&str, String) {
    // Handle twigils: $*, $?, $!, $^
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest_after_name, name) = parse_qualified_ident_with_hyphens_or_empty(rest);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    (rest_after_name, full_name)
}

/// Parse a $variable reference.
pub(super) fn scalar_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '$')?;
    // Handle $(stmt; expr) — statement block in scalar context
    // Try to parse as a statement list first (for cases like `$(let $a = 23; $a)`)
    if let Some(inner) = input.strip_prefix('(') {
        if let Ok(result) = parse_dollar_paren_block(inner) {
            return Ok(result);
        }
        return paren_expr(input);
    }
    // Handle $@array (array in item context) and $%hash (hash in item context)
    if input.starts_with('@') || input.starts_with('%') {
        let sigil = &input[..1];
        let after_sigil = &input[1..];
        // Check for twigils or plain identifier
        let (rest, twigil) = if after_sigil.starts_with('*')
            || after_sigil.starts_with('?')
            || after_sigil.starts_with('!')
        {
            (&after_sigil[1..], &after_sigil[..1])
        } else {
            (after_sigil, "")
        };
        if let Ok((rest, name)) = parse_qualified_ident_with_hyphens(rest) {
            let full_name = if twigil.is_empty() {
                name
            } else {
                format!("{}{}", twigil, name)
            };
            return if sigil == "@" {
                Ok((rest, Expr::ArrayVar(full_name)))
            } else {
                Ok((rest, Expr::HashVar(full_name)))
            };
        }
    }
    // Handle $_ special variable
    if input.starts_with('_')
        && (input.len() == 1 || !input[1..].chars().next().unwrap().is_alphanumeric())
    {
        return Ok((&input[1..], Expr::Var("_".to_string())));
    }
    // Handle $/ (match variable)
    if let Some(stripped) = input.strip_prefix('/') {
        return Ok((stripped, Expr::Var("/".to_string())));
    }
    // Handle $! (exception variable) — bare $! without name after it
    if let Some(after) = input.strip_prefix('!') {
        // If next char is alphanumeric or _, it's a twigil (e.g. $!attr)
        // If not, it's the bare $! variable
        let is_twigil = !after.is_empty()
            && after
                .chars()
                .next()
                .is_some_and(|c| c.is_alphanumeric() || c == '_');
        if !is_twigil {
            return Ok((after, Expr::Var("!".to_string())));
        }
    }
    // Handle $<name> (named capture variable), including forms like $<&foo>.
    if let Some(after_lt) = input.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let name = &after_lt[..end];
        if !name.is_empty() {
            let after_gt = &after_lt[end + 1..];
            return Ok((after_gt, Expr::CaptureVar(name.to_string())));
        }
    }
    // Handle $=finish and other Pod variables ($=pod, $=data, etc.)
    if let Some(after_eq) = input.strip_prefix('=') {
        let (rest, name) = parse_ident_with_hyphens(after_eq)?;
        let full_name = format!("={}", name);
        return Ok((rest, Expr::Var(full_name)));
    }
    // Named parameter variable inside blocks: $:name
    if let Some(after_colon) = input.strip_prefix(':') {
        let (rest, name) = parse_ident_with_hyphens(after_colon)?;
        return Ok((rest, Expr::Var(format!(":{}", name))));
    }
    // Handle bare $ (anonymous state variable)
    // If next char is not a valid identifier start, twigil, or special char, it's an anonymous var
    let next_is_ident_or_twigil = !input.is_empty() && {
        let first_char = input.chars().next().unwrap();
        first_char.is_alphanumeric()
            || first_char == '_'
            || first_char == '*'
            || first_char == '?'
            || first_char == '!'
            || first_char == '^'
            || first_char == '.'
    };
    if !next_is_ident_or_twigil {
        return Ok((input, Expr::Var("__ANON_STATE__".to_string())));
    }
    // $. followed by non-alphabetic: shorthand for self. (e.g., $.^name = self.^name)
    // Return "self" as a BareWord and leave the '.' for postfix parsing.
    if input.starts_with('.')
        && input.len() > 1
        && !input.as_bytes()[1].is_ascii_alphabetic()
        && input.as_bytes()[1] != b'_'
    {
        return Ok((input, Expr::BareWord("self".to_string())));
    }
    // $.ident followed by ( or : → parse as self.ident(args) or self.ident: args
    // This ensures $.method(args) works as a method call, not a variable call.
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        // Look ahead past the identifier to check for ( or :
        let after_dot = &input[1..];
        let ident_end = after_dot
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(after_dot.len());
        let after_ident = &after_dot[ident_end..];
        if after_ident.starts_with('(')
            || (after_ident.starts_with(':') && !after_ident.starts_with("::"))
        {
            return Ok((input, Expr::BareWord("self".to_string())));
        }
    }
    // Handle twigils: $*FOO, $?FILE, $!attr, $.attr
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('.') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    // $?LINE is a compile-time constant: replace with the current line number
    if full_name == "?LINE" {
        let line = current_line_number(input);
        return Ok((rest, Expr::Literal(Value::Int(line))));
    }
    Ok((rest, Expr::Var(full_name)))
}

/// Parse identifier allowing kebab-case hyphens and apostrophes (e.g., `my-var`, `same'proto`).
/// A hyphen/apostrophe is only part of the name if followed by an alphabetic char or `_`,
/// so `$pd--` parses as `$pd` + postfix `--`.
pub(super) fn parse_ident_with_hyphens<'a>(input: &'a str) -> PResult<'a, &'a str> {
    let (rest, _first) = take_while1(input, |c: char| c.is_alphanumeric() || c == '_')?;
    let mut end = input.len() - rest.len();
    loop {
        let remaining = &input[end..];
        let sep = if remaining.starts_with('-') {
            Some('-')
        } else if remaining.starts_with('\'') {
            Some('\'')
        } else {
            None
        };
        if let Some(sep) = sep {
            let after_sep = &remaining[sep.len_utf8()..];
            if let Some(next) = after_sep.chars().next()
                && (next.is_alphabetic() || next == '_')
            {
                end += sep.len_utf8();
                for c in after_sep.chars() {
                    if c.is_alphanumeric() || c == '_' {
                        end += c.len_utf8();
                    } else {
                        break;
                    }
                }
                continue;
            }
        }
        break;
    }
    Ok((&input[end..], &input[..end]))
}

fn parse_qualified_ident_with_hyphens<'a>(input: &'a str) -> PResult<'a, String> {
    let (mut rest, first) = parse_ident_with_hyphens(input)?;
    let mut full = first.to_string();
    while let Some(after) = rest.strip_prefix("::") {
        let (r2, part) = parse_ident_with_hyphens(after)?;
        full.push_str("::");
        full.push_str(part);
        rest = r2;
    }
    Ok((rest, full))
}

fn parse_qualified_ident_with_hyphens_or_empty(input: &str) -> (&str, String) {
    if let Ok((rest, name)) = parse_qualified_ident_with_hyphens(input) {
        (rest, name)
    } else {
        (input, String::new())
    }
}

/// Parse an @array variable reference.
pub(super) fn array_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '@')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Bare @ (anonymous array variable)
    let next_is_ident = !rest.is_empty()
        && rest
            .chars()
            .next()
            .is_some_and(|c| c.is_alphanumeric() || c == '_');
    if !next_is_ident && twigil.is_empty() {
        return Ok((rest, Expr::ArrayVar("__ANON_ARRAY__".to_string())));
    }
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
pub(super) fn hash_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*') || input.starts_with('!') {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Bare % (anonymous hash variable)
    let next_is_ident = !rest.is_empty()
        && rest
            .chars()
            .next()
            .is_some_and(|c| c.is_alphanumeric() || c == '_');
    if !next_is_ident && twigil.is_empty() {
        return Ok((rest, Expr::HashVar("__ANON_HASH__".to_string())));
    }
    // Special: %*ENV
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Check if an identifier is a known pseudo-package name.
pub(crate) fn is_pseudo_package(name: &str) -> bool {
    matches!(
        name,
        "SETTING" | "CALLER" | "OUTER" | "CORE" | "GLOBAL" | "MY" | "OUR" | "DYNAMIC" | "UNIT"
    )
}

/// Parse a &code variable reference.
/// Handles `&foo`, twigil forms `&?BLOCK`, operator references like `&infix:<+>`,
/// package-qualified code refs like `&SETTING::OUTER::name`, and indirect package
/// lookups like `&::($expr)::name` or `&CALLER::($expr)::name`.
pub(super) fn code_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '&')?;
    // Handle &[op] — short form for &infix:<op>
    if let Some(after_bracket) = input.strip_prefix('[')
        && let Some(end_pos) = after_bracket.find(']')
    {
        let op_name = &after_bracket[..end_pos];
        let rest = &after_bracket[end_pos + 1..];
        let full_name = format!("infix:<{}>", op_name);
        return Ok((rest, Expr::CodeVar(full_name)));
    }
    // Handle &CALLER::($expr)::name — CALLER prefix followed by indirect lookup
    if let Some(after_caller) = input.strip_prefix("CALLER::(") {
        let (after_expr, expr) = expression(after_caller)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: name.to_string(),
                },
            ));
        }
        return Err(PError::expected(":: after indirect package lookup"));
    }
    // Handle &::($expr)::name — indirect package lookup
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = expression(after_colons)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: name.to_string(),
                },
            ));
        }
        return Err(PError::expected(":: after indirect package lookup"));
    }
    // Handle package-qualified code refs: &SETTING::OUTER::...::name
    // or &CALLER::SETTING::OUTER::...::name
    if let Ok((_, first_ident)) = parse_ident_with_hyphens(input)
        && is_pseudo_package(first_ident)
    {
        let after_first = &input[first_ident.len()..];
        if after_first.starts_with("::") {
            // Consume the full Package::Package::...::name chain
            let mut pos = 0;
            let mut qualified = String::new();
            loop {
                if let Ok((_, ident)) = parse_ident_with_hyphens(&input[pos..]) {
                    let after_ident = &input[pos + ident.len()..];
                    if after_ident.starts_with("::") && is_pseudo_package(ident) {
                        // This is a pseudo-package, consume it and the ::
                        qualified.push_str(ident);
                        qualified.push_str("::");
                        pos += ident.len() + 2;
                        continue;
                    }
                    // This is the final name
                    qualified.push_str(ident);
                    pos += ident.len();
                    return Ok((&input[pos..], Expr::CodeVar(qualified)));
                }
                break;
            }
        }
    }
    // Handle twigils on code vars: &?BLOCK, &!DISPATCHER, &^x, &*FOO
    let (rest, twigil) = if let Some(stripped) = input.strip_prefix('?') {
        (stripped, "?")
    } else if let Some(stripped) = input.strip_prefix('!') {
        (stripped, "!")
    } else if let Some(stripped) = input.strip_prefix('^') {
        (stripped, "^")
    } else if let Some(stripped) = input.strip_prefix('*') {
        (stripped, "*")
    } else {
        (input, "")
    };
    let (rest, name) = parse_ident_with_hyphens(rest)?;
    // Check for operator reference: &infix:<OP>, &prefix:<OP>, &postfix:<OP>
    if twigil.is_empty() && matches!(name, "infix" | "prefix" | "postfix") && rest.starts_with(":<")
    {
        let r = &rest[2..]; // skip ':' and '<'
        if let Some(end_pos) = r.find('>') {
            let op_name = &r[..end_pos];
            let r = &r[end_pos + 1..];
            let full_name = format!("{}:<{}>", name, op_name);
            return Ok((r, Expr::CodeVar(full_name)));
        }
    }
    let full_name = if twigil.is_empty() {
        name.to_string()
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::CodeVar(full_name)))
}

/// Parse `$(stmt; stmt; ... expr)` — statement block in scalar context.
/// Returns DoBlock expression.
fn parse_dollar_paren_block(input: &str) -> PResult<'_, Expr> {
    use super::super::helpers::ws;
    // Parse first statement
    let (mut rest, _) = ws(input)?;
    let mut stmts = Vec::new();
    loop {
        if rest.starts_with(')') || rest.is_empty() {
            break;
        }
        let (r, stmt) = super::super::stmt::statement_pub(rest)?;
        stmts.push(stmt);
        let (r, _) = ws(r)?;
        // Consume semicolons
        let mut r = r;
        while r.starts_with(';') {
            r = &r[1..];
            let (r2, _) = ws(r)?;
            r = r2;
        }
        rest = r;
    }
    if stmts.len() < 2 {
        return Err(PError::expected("statement block with multiple statements"));
    }
    let (rest, _) = parse_char(rest, ')')?;
    Ok((
        rest,
        Expr::DoBlock {
            body: stmts,
            label: None,
        },
    ))
}
