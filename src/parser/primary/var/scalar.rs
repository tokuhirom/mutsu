/// Scalar variable parsing ($-sigil) and related block forms.
///
/// Covers `$var`, `$*dyn`, `$?compile`, `$.attr`, `$<cap>`, `$(block)`,
/// symbolic derefs `$::("name")`, and variable name parsing for interpolation.
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::Expr;
use crate::parser::helpers::{is_raku_identifier_continue, is_raku_identifier_start, ws};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::adverb::parse_var_name_adverb_suffixes;
use super::ident::{
    parse_ident_with_hyphens, parse_interpolation_qualified_ident_with_hyphens_or_empty,
    parse_qualified_ident_prefix_with_hyphens, parse_qualified_ident_with_hyphens,
};
use super::perl5::detect_perl5_scalar_var;

static ANON_STATE_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Build a string concatenation expression: left ~ right
fn concat_exprs(left: Expr, right: Expr) -> Expr {
    Expr::Binary {
        left: Box::new(left),
        op: TokenKind::Tilde,
        right: Box::new(right),
    }
}

fn parse_symbolic_deref_segments(mut rest: &str, mut combined: Expr) -> PResult<'_, Expr> {
    loop {
        if let Some(after_sep) = rest.strip_prefix("::(") {
            combined = concat_exprs(combined, Expr::Literal(Value::str("::".to_string())));
            let (after_expr, seg_expr) = crate::parser::expr::expression(after_sep)?;
            let (after_expr, _) = ws(after_expr)?;
            let (r, _) = parse_char(after_expr, ')')?;
            combined = concat_exprs(combined, seg_expr);
            rest = r;
        } else if let Some(after_sep) = rest.strip_prefix("::") {
            if let Ok((r, ident)) = parse_ident_with_hyphens(after_sep) {
                let sep_ident = format!("::{}", ident);
                combined = concat_exprs(combined, Expr::Literal(Value::str(sep_ident)));
                rest = r;
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((rest, combined))
}

/// Parse a variable name from raw string (used in interpolation).
pub(crate) fn parse_var_name_from_str(input: &str) -> (&str, String) {
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
    let (rest_after_name, name) = parse_interpolation_qualified_ident_with_hyphens_or_empty(rest);
    let (rest_after_name, name) = parse_var_name_adverb_suffixes(rest_after_name, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    (rest_after_name, full_name)
}

/// Parse a $variable reference.
pub(crate) fn scalar_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '$')?;
    // Detect Perl 5 special variables and throw X::Syntax::Perl5Var errors.
    if let Some(err) = detect_perl5_scalar_var(input) {
        return Err(PError::fatal(err));
    }
    // Handle $(stmt; expr) — statement block in scalar context
    // Try to parse as a statement list first (for cases like `$(let $a = 23; $a)`)
    if let Some(inner) = input.strip_prefix('(') {
        // Handle $(;) — semicolons only inside $() produce an empty itemized list
        {
            let mut r = inner;
            loop {
                r = r.trim_start();
                if let Some(rest) = r.strip_prefix(';') {
                    r = rest;
                } else {
                    break;
                }
            }
            r = r.trim_start();
            if let Some(rest) = r.strip_prefix(')') {
                return Ok((
                    rest,
                    Expr::MethodCall {
                        target: Box::new(Expr::ArrayLiteral(Vec::new())),
                        name: Symbol::intern("item"),
                        args: vec![],
                        modifier: None,
                        quoted: false,
                    },
                ));
            }
        }
        if let Ok(result) = parse_dollar_paren_block(inner) {
            return Ok(result);
        }
        return crate::parser::primary::container::paren_expr(input);
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
            let inner = if sigil == "@" {
                Expr::ArrayVar(full_name)
            } else {
                Expr::HashVar(full_name)
            };
            return Ok((rest, Expr::Itemize(Box::new(inner))));
        }
    }
    // Handle nested scalar dereference syntax ($$x / $&f) by parsing the
    // inner variable term. This keeps '$' from being misparsed as an
    // anonymous bare-sigil state variable in expressions like `is($$o, ...)`.
    if input.starts_with('$') {
        return scalar_var(input);
    }
    if input.starts_with('&') {
        return super::sigil_vars::code_var(input);
    }
    // Handle $_ special variable
    if input.starts_with('_')
        && (input.len() == 1
            || !input[1..]
                .chars()
                .next()
                .is_some_and(is_raku_identifier_continue))
    {
        return Ok((&input[1..], Expr::Var("_".to_string())));
    }
    // Handle $/ (match variable)
    if let Some(stripped) = input.strip_prefix('/') {
        return Ok((stripped, Expr::Var("/".to_string())));
    }
    // Handle $¢ (current match cursor variable)
    if let Some(stripped) = input.strip_prefix('\u{00A2}') {
        return Ok((stripped, Expr::Var("\u{00A2}".to_string())));
    }
    // Handle $! (exception variable) — bare $! without name after it
    if let Some(after) = input.strip_prefix('!') {
        // If next char is alphanumeric or _, it's a twigil (e.g. $!attr)
        // If not, it's the bare $! variable
        let is_twigil =
            !after.is_empty() && after.chars().next().is_some_and(is_raku_identifier_start);
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
        // `$<>` is the zen slice of the match variable: sugar for `$/<>`.
        // Return `$/` and leave the `<>` for postfix zen-angle parsing.
        return Ok((input, Expr::Var("/".to_string())));
    }
    // Handle $=finish and other Pod variables ($=pod, $=data, etc.)
    if let Some(after_eq) = input.strip_prefix('=') {
        let (rest, name) = parse_ident_with_hyphens(after_eq)?;
        let full_name = format!("={}", name);
        return Ok((rest, Expr::Var(full_name)));
    }
    // Symbolic variable dereference: $::("name"), $::("pkg")::name::($dyn)
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, first_expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = ws(after_expr)?;
        let (rest, _) = parse_char(after_expr, ')')?;
        let (rest, combined) = parse_symbolic_deref_segments(rest, first_expr)?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "$".to_string(),
                expr: Box::new(combined),
            },
        ));
    }
    if let Some(after_brace) = input.strip_prefix("::{") {
        let (rest, _) = ws(after_brace)?;
        let (rest, key_expr) = crate::parser::expr::expression(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, '}')?;
        return Ok((
            rest,
            Expr::Index {
                target: Box::new(Expr::PseudoStash("::".to_string())),
                index: Box::new(key_expr),
                is_positional: false,
            },
        ));
    }
    if let Some(after_bracket) = input.strip_prefix("::<")
        && let Some(end) = after_bracket.find('>')
    {
        let symbol = &after_bracket[..end];
        if !symbol.is_empty() {
            return Ok((
                &after_bracket[end + 1..],
                Expr::Index {
                    target: Box::new(Expr::PseudoStash("::".to_string())),
                    index: Box::new(Expr::Literal(Value::str(symbol.to_string()))),
                    is_positional: false,
                },
            ));
        }
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
        is_raku_identifier_start(first_char)
            || first_char.is_numeric()
            || first_char == '*'
            || first_char == '?'
            || first_char == '!'
            || first_char == '^'
            || first_char == '.'
            || first_char == '~'
    };
    if !next_is_ident_or_twigil {
        let id = ANON_STATE_COUNTER.fetch_add(1, Ordering::Relaxed);
        return Ok((input, Expr::Var(format!("__ANON_STATE_{id}__"))));
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
            .find(|c: char| !is_raku_identifier_continue(c) && c != '-')
            .unwrap_or(after_dot.len());
        let after_ident = &after_dot[ident_end..];
        if after_ident.starts_with('(')
            || (after_ident.starts_with(':') && !after_ident.starts_with("::"))
        {
            return Ok((input, Expr::BareWord("self".to_string())));
        }
    }
    // Handle twigils: $*FOO, $?FILE, $!attr, $.attr, $~MAIN
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('.')
            && input.len() > 1
            && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_'))
        || (input.starts_with('~') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    let (rest, name) = parse_qualified_ident_prefix_with_hyphens(rest)?;
    if rest.starts_with("::(") {
        let (rest, combined) =
            parse_symbolic_deref_segments(rest, Expr::Literal(Value::str(name.clone())))?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "$".to_string(),
                expr: Box::new(combined),
            },
        ));
    }
    let (rest, name) = parse_var_name_adverb_suffixes(rest, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    // $?LINE is a compile-time constant: replace with the current line number
    if full_name == "?LINE" {
        let line = crate::parser::primary::current_line_number(input);
        return Ok((rest, Expr::Literal(Value::Int(line))));
    }
    // $?TABSTOP is a compile-time constant: hardcoded to 8
    if full_name == "?TABSTOP" {
        return Ok((rest, Expr::Literal(Value::Int(8))));
    }
    // Normalize positional capture variables: $00 → $0, $01 → $1, etc.
    // Strip leading zeros from all-digit variable names so $00 resolves to $0.
    let full_name = if !full_name.is_empty()
        && full_name.chars().next().unwrap().is_ascii_digit()
        && full_name.chars().all(|c| c.is_ascii_digit())
    {
        let trimmed = full_name.trim_start_matches('0');
        if trimmed.is_empty() {
            "0".to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        full_name
    };
    Ok((rest, Expr::Var(full_name)))
}

/// Parse `$(stmt; stmt; ... expr)` — statement block in scalar context.
/// Returns DoBlock expression.
pub(crate) fn parse_dollar_paren_block_pub(input: &str) -> PResult<'_, Expr> {
    parse_dollar_paren_block(input)
}

fn parse_dollar_paren_block(input: &str) -> PResult<'_, Expr> {
    // Parse first statement
    let (mut rest, _) = ws(input)?;
    let mut stmts = Vec::new();
    loop {
        if rest.starts_with(')') || rest.is_empty() {
            break;
        }
        let (r, stmt) = crate::parser::stmt::statement_pub(rest)?;
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
