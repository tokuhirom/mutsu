use super::super::super::expr::expression;
use super::super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::super::parse_result::{PError, PResult, parse_char, take_while1};
use super::super::{ident, keyword, qualified_ident};
use super::take_while_opt;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;
use crate::value::ValueView;

/// Record an enum's value names, so later parsing knows each is a *complete*
/// nullary term rather than a possible listop head (see
/// [`is_user_declared_enum_value`](crate::parser::stmt::simple::is_user_declared_enum_value)).
///
/// The `__DYNAMIC__` placeholder the computed-body forms emit is not a value
/// name, and a value spelled like a parser keyword is left alone for the same
/// reason declaration helpers do.
fn register_enum_values(variants: &[(String, Option<Expr>)]) {
    for (name, _) in variants {
        if name == "__DYNAMIC__" || name.is_empty() {
            continue;
        }
        super::super::simple::register_user_enum_value(name);
    }
}

/// Skip a balanced `[...]` role-parameterization argument. Returns the input
/// past the closing `]`, or `None` when the input does not start with `[`.
fn skip_balanced_brackets(input: &str) -> Option<&str> {
    let mut rest = input.strip_prefix('[')?;
    let mut depth = 1u32;
    while depth > 0 {
        let c = rest.chars().next()?;
        rest = &rest[c.len_utf8()..];
        match c {
            '[' => depth += 1,
            ']' => depth -= 1,
            _ => {}
        }
    }
    Some(rest)
}

/// Parse `anon enum` declaration.
pub(crate) fn anon_enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("anon", input).ok_or_else(|| PError::expected("anon enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("enum", rest).ok_or_else(|| PError::expected("enum after anon"))?;
    let (rest, _) = ws1(rest)?;
    parse_anon_enum_body(rest)
}

/// Parse `enum` declaration.
pub(crate) fn enum_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("enum", input).ok_or_else(|| PError::expected("enum declaration"))?;
    let (rest, _) = ws1(rest)?;
    // Anonymous enum: `enum < foo bar >` or `enum :: < foo bar >`
    if rest.starts_with('<') || rest.starts_with('(') {
        return parse_anon_enum_body(rest);
    }
    if let Some(r) = rest.strip_prefix("::") {
        let (r, _) = ws(r)?;
        if r.starts_with('<') || r.starts_with('(') {
            return parse_anon_enum_body(r);
        }
    }
    parse_enum_decl_body(rest, false)
}

/// Parse anonymous enum body (after `enum` keyword with no name).
fn parse_anon_enum_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, variants) = if input.starts_with("<<") || input.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(input)?
    } else if input.starts_with('<') {
        let (r, _) = parse_char(input, '<')?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            let (r2, _) =
                take_while_opt(r, |c: char| c == ' ' || c == '\t' || c == '\n' || c == '\r');
            if let Some(r2) = r2.strip_prefix('>') {
                r = r2;
                break;
            }
            let (r2, word) = take_while1(r2, |c: char| {
                c != '>' && c != ' ' && c != '\t' && c != '\n' && c != '\r'
            })?;
            variants.push((word.to_string(), None));
            r = r2;
        }
        (r, variants)
    } else if input.starts_with('(') {
        let (r, body) = parse_paren_enum_body(input)?;
        let variants = enum_variants_from_body(&body)
            .ok_or_else(|| PError::expected("anonymous enum variants"))?;
        (r, variants)
    } else {
        return Err(PError::expected("anonymous enum variants"));
    };
    let (rest, _) = ws(rest)?;
    // Do NOT consume a trailing `;` here: the statement layer's
    // `consume_semicolons` already handles the terminator. Eating it in this
    // parser breaks the expression-context use (`my $e = enum <a b c>; say $x`),
    // where the swallowed `;` lets the expression parser absorb the following
    // statement as an infix continuation. (The dynamic-enum paths above already
    // return without consuming `;`.)
    register_enum_values(&variants);
    Ok((
        rest,
        Stmt::EnumDecl {
            name: Symbol::intern(""),
            variants,
            is_export: false,
            is_my: false,
            base_type: None,
            roles: Vec::new(),
            language_version: super::super::simple::current_language_version(),
        },
    ))
}

/// Parse `<< ... >>` or `\u{ab} ... \u{bb}` enum variant list.
/// Supports plain words and colonpairs like `:key<value>` or `:key[expr, ...]`.
fn parse_double_angle_enum_variants(input: &str) -> PResult<'_, Vec<(String, Option<Expr>)>> {
    let (r, use_unicode_close) = if let Some(r) = input.strip_prefix("<<") {
        (r, false)
    } else if let Some(r) = input.strip_prefix('\u{ab}') {
        // «
        (r, true)
    } else {
        return Err(PError::expected("<< or \u{ab}"));
    };
    let mut variants = Vec::new();
    let mut r = r;
    loop {
        // Skip whitespace (including newlines)
        let (r2, _) = ws(r)?;
        r = r2;
        // Check for closing >> or »
        if use_unicode_close {
            if let Some(r2) = r.strip_prefix('\u{bb}') {
                return Ok((r2, variants));
            }
        } else if let Some(r2) = r.strip_prefix(">>") {
            return Ok((r2, variants));
        }
        // Colonpair: :key<value> or :key[expr, ...] or :!key
        if r.starts_with(':') && !r.starts_with("::") {
            let after_colon = &r[1..];
            // Handle :!key (negated boolean)
            let (after_neg, negated) = if let Some(stripped) = after_colon.strip_prefix('!') {
                (stripped, true)
            } else {
                (after_colon, false)
            };
            // Parse the key (identifier)
            let (after_key, key) = take_while1(after_neg, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            if negated {
                // :!key => value is 0 (false)
                variants.push((key.to_string(), Some(Expr::Literal(Value::int(0.into())))));
                r = after_key;
            } else if let Some(after_open) = after_key.strip_prefix('<') {
                // :key<value>
                let (after_val, val) = take_while1(after_open, |c: char| c != '>')?;
                let after_close = after_val
                    .strip_prefix('>')
                    .ok_or_else(|| PError::expected(">"))?;
                variants.push((key.to_string(), Some(Expr::Literal(Value::str_from(val)))));
                r = after_close;
            } else if after_key.starts_with('[') {
                // :key[expr, ...] — parse as array expression
                let (after_expr, expr) = expression(after_key)?;
                variants.push((key.to_string(), Some(expr)));
                r = after_expr;
            } else if after_key.starts_with('(') {
                // :key(expr) — parse the balanced-paren content as an expression.
                // Parse only the text INSIDE the parens (via the balanced span),
                // not `expression(after_key)` on the raw remainder: otherwise the
                // enum's closing `»`/`>>` delimiter right after `)` is swallowed
                // as a hyper operator (`«:one(1)»` mis-parsed as `(1)».(...)`).
                let after_parens = skip_balanced_parens(after_key);
                let inner = &after_key[1..after_key.len() - after_parens.len() - 1];
                let (_, expr) = expression(inner.trim())?;
                variants.push((key.to_string(), Some(expr)));
                r = after_parens;
            } else {
                // :key with no value — treat as boolean true (no explicit value)
                variants.push((key.to_string(), None));
                r = after_key;
            }
        } else {
            // Plain word
            let close_char = if use_unicode_close { '\u{bb}' } else { '>' };
            let (r2, word) = take_while1(r, |c: char| {
                !c.is_whitespace() && c != close_char && c != '>' && c != ':'
            })?;
            variants.push((word.to_string(), None));
            r = r2;
        }
    }
}

/// Parse an `enum`'s parenthesized body with the ordinary parenthesized-term
/// grammar rule.
///
/// Rakudo's `enum` does not have a variant-list grammar of its own: the body is
/// just a term, so every separator the parenthesized term supports works here.
/// In particular a top-level `;` splits the term into sections
/// (`enum Foo (A => 0; B => 10)`, and the multi-line spelling used by
/// `Language/nativecall.rakudoc`), a trailing `,`/`;` is a terminator rather
/// than an extra element, and computed bodies (`1..5 Z=> 'a'..'e'`, `%hash`)
/// come back as a single expression.
fn parse_paren_enum_body(input: &str) -> PResult<'_, Expr> {
    crate::parser::primary::container::paren_expr(input)
}

/// Turn one element of an already-parsed enum body into a static variant.
///
/// `None` means the element is not a plain name / `name => value` pair, so the
/// whole body has to stay a computed (`__DYNAMIC__`) expression.
fn enum_variant_from_expr(expr: Expr) -> Option<(String, Option<Expr>)> {
    match expr {
        // A *bare identifier* inside the parenthesised `(...)` enum body is a
        // term reference, not an autoquoted key (only the `<...>` word-list form
        // autoquotes). If it is not a declared symbol it is X::Undeclared — so
        // reject it here and let the computed-body fallback keep the expression,
        // which the undeclared-names check scans.
        // (Pairs like `A => 1` keep their bare LHS as an autoquoted key below.)
        Expr::BareWord(_) => None,
        Expr::Literal(lit) if lit.as_str().is_some() => {
            Some((lit.as_str().unwrap().to_string(), None))
        }
        // A single pair is wrapped as a positional pair by the term parser.
        Expr::PositionalPair(inner) => enum_variant_from_expr(*inner),
        Expr::Binary {
            left,
            op: crate::token_kind::TokenKind::FatArrow,
            right,
        } => match *left {
            Expr::Literal(lit) if lit.as_str().is_some() => {
                let name = lit.as_str().unwrap().to_string();
                let value_expr = match *right {
                    Expr::Literal(rl) if matches!(rl.view(), ValueView::Bool(true)) => None,
                    other => Some(other),
                };
                Some((name, value_expr))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Decompose a parsed `(...)` enum body into static variants, or `None` when at
/// least one element is not a static name/pair (the caller then keeps the body
/// as a computed expression).
fn enum_variants_from_body(body: &Expr) -> Option<Vec<(String, Option<Expr>)>> {
    match body {
        Expr::ArrayLiteral(items) => items
            .iter()
            .cloned()
            .map(enum_variant_from_expr)
            .collect::<Option<Vec<_>>>(),
        other => enum_variant_from_expr(other.clone()).map(|variant| vec![variant]),
    }
}

pub(in crate::parser::stmt) fn parse_enum_decl_body(input: &str, is_my: bool) -> PResult<'_, Stmt> {
    parse_enum_decl_body_with_type(input, None, is_my)
}

pub(super) fn parse_enum_decl_body_with_type(
    input: &str,
    base_type: Option<String>,
    is_my: bool,
) -> PResult<'_, Stmt> {
    let (rest, name_str) = qualified_ident(input)?;
    let name = Symbol::intern(&name_str);
    // An enum's *name* is a type, exactly like a class or role name. Register
    // it as one so a bareword `Day` (or the `Day::Mon` value spelling, whose
    // head must resolve to the enum type) is recognized as declared — the
    // module-scan harvest already collects enum names this way for imported
    // enums; this is the same for the file being parsed.
    super::super::simple::register_user_type(&name_str);
    let (rest, _) = ws(rest)?;

    // Parse the declaration's trait clauses — `is <trait>` (e.g. `is export`)
    // and `does <Role>` — which may appear in any order and repeat
    // (`enum E does A does B is export <x y>`). Without the `does` arm the
    // clause was left unconsumed, the `(...)`/`<...>` body was never read as the
    // enum's value list, and the leftover `does Role (A => 1, B => 2)` parsed as
    // a plain expression statement — which is where the spurious
    // "Useless use of '=>' in sink context" warning came from, and why the enum
    // ended up with no values at all.
    let mut rest = rest;
    let mut is_export = false;
    let mut roles: Vec<String> = Vec::new();
    loop {
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            let (r, trait_name) = ident(r)?;
            if trait_name == "export" {
                is_export = true;
            }
            // Consume an optional parenthesized trait argument, e.g.
            // `is export(:traits)` — without this, the variant parser mistakes
            // the `(:traits)` for the enum's `(...)` body and the real
            // `<values>` list after it is left dangling.
            let r = super::super::super::helpers::skip_balanced_parens(r);
            let (r, _) = ws(r)?;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("does", rest) {
            let (r, _) = ws1(r)?;
            let (r, role_name) = qualified_ident(r)?;
            // A parameterized role (`does R[Int]`) keeps its argument list in
            // the recorded name, the same spelling class composition uses.
            let (r, role_name) = match skip_balanced_brackets(r) {
                Some(after) => {
                    let consumed = &r[..r.len() - after.len()];
                    (after, format!("{role_name}{consumed}"))
                }
                None => (r, role_name),
            };
            roles.push(role_name);
            let (r, _) = ws(r)?;
            rest = r;
            continue;
        }
        break;
    }

    // Enum variants in << >>, « », <> or ()
    let (rest, variants) = if rest.starts_with("<<") || rest.starts_with('\u{ab}') {
        parse_double_angle_enum_variants(rest)?
    } else if rest.starts_with('<') {
        let (r, _) = parse_char(rest, '<')?;
        let mut variants = Vec::new();
        let mut r = r;
        loop {
            let (r2, _) =
                take_while_opt(r, |c: char| c == ' ' || c == '\t' || c == '\n' || c == '\r');
            if let Some(r2) = r2.strip_prefix('>') {
                r = r2;
                break;
            }
            let (r2, word) = take_while1(r2, |c: char| {
                c != '>' && c != ' ' && c != '\t' && c != '\n' && c != '\r'
            })?;
            variants.push((word.to_string(), None));
            r = r2;
        }
        (r, variants)
    } else if rest.starts_with('(') {
        // The body is an ordinary parenthesized term (see `parse_paren_enum_body`).
        // A failure here is a real syntax error in a construct we are already
        // committed to (`enum <Name> (`), so report it instead of letting the
        // statement layer backtrack and re-read the `enum` keyword as a call to
        // an undeclared routine — that produced the useless
        // "Undeclared routine: enum used" for every malformed enum body.
        let (r, body) = parse_paren_enum_body(rest).map_err(|err| {
            if err.is_fatal() {
                err
            } else {
                PError::fatal_at(format!("Malformed enum body for enum '{name_str}'"), rest)
            }
        })?;
        match enum_variants_from_body(&body) {
            Some(variants) => (r, variants),
            None => {
                // Computed body (operators like `X~`, `Z=>`, `|`, a `%hash`, …):
                // keep the whole expression and let the runtime build the enum.
                return Ok((
                    r,
                    Stmt::EnumDecl {
                        name,
                        variants: vec![("__DYNAMIC__".to_string(), Some(body))],
                        is_export,
                        is_my,
                        base_type: base_type.clone(),
                        roles,
                        language_version: super::super::simple::current_language_version(),
                    },
                ));
            }
        }
    } else {
        (rest, Vec::new())
    };

    let (rest, _) = ws(rest)?;
    // See parse_anon_enum_body: leave the trailing `;` for the statement layer
    // so `enum` in expression context does not swallow the next statement.
    register_enum_values(&variants);
    Ok((
        rest,
        Stmt::EnumDecl {
            name,
            variants,
            is_export,
            is_my,
            base_type,
            roles,
            language_version: super::super::simple::current_language_version(),
        },
    ))
}
