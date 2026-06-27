use crate::ast::{Expr, Stmt};
use crate::parser::expr::{expression, expression_no_sequence};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char, parse_tag};
use crate::parser::primary::misc::parse_block_body;
use crate::symbol::Symbol;
use crate::value::Value;

fn is_superscript_digit(c: char) -> bool {
    matches!(
        c,
        '\u{2070}' | '\u{00B9}' | '\u{00B2}' | '\u{00B3}' | '\u{2074}'..='\u{2079}'
    )
}

pub(crate) fn declared_term_symbol(input: &str) -> PResult<'_, Expr> {
    if let Some((name, consumed_len, callable)) =
        crate::parser::stmt::simple::match_user_declared_term_symbol(input)
    {
        // If the declared callable is immediately followed by `(`, defer to
        // identifier_or_call so `name(...)` parses as a single call expression.
        //
        // The same applies to a non-callable term (e.g. a sigilless `constant`)
        // whose name also names a user-declared sub: parens always indicate a
        // sub call, so `name()` must dispatch to the sub rather than evaluate
        // the constant term and call its value.
        if input[consumed_len..].starts_with('(')
            && (callable || crate::parser::stmt::simple::is_user_declared_sub(&name))
        {
            return Err(PError::expected("declared term symbol"));
        }
        let expr = if callable {
            Expr::Call {
                name: Symbol::intern(&name),
                args: vec![],
            }
        } else {
            Expr::BareWord(name)
        };
        return Ok((&input[consumed_len..], expr));
    }
    Err(PError::expected("declared term symbol"))
}

/// Parse `::Foo` class literal (type object reference) or `::($expr)` indirect type lookup.
pub(crate) fn class_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("::") {
        return Err(PError::expected("class literal"));
    }
    let rest = &input[2..];
    if rest.is_empty()
        || rest.starts_with('.')
        || rest.starts_with(';')
        || rest.starts_with(')')
        || rest.starts_with(',')
        || rest.starts_with(' ')
        || rest.starts_with('\n')
        || rest.starts_with('\r')
        || rest.starts_with('\t')
        || rest.starts_with('}')
    {
        return Ok((rest, Expr::PseudoStash("::".to_string())));
    }
    if let Some(after_bracket) = rest.strip_prefix('<')
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
    if let Some(after_brace) = rest.strip_prefix('{') {
        let (r, _) = ws(after_brace)?;
        let (r, key_expr) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, '}')?;
        return Ok((
            r,
            Expr::Index {
                target: Box::new(Expr::PseudoStash("::".to_string())),
                index: Box::new(key_expr),
                is_positional: false,
            },
        ));
    }
    // Handle ::?PACKAGE and ::?MODULE — compile-time pseudo-packages
    if let Some(after) = rest.strip_prefix("?PACKAGE")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?PACKAGE".to_string())));
    }
    if let Some(after) = rest.strip_prefix("?MODULE")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?MODULE".to_string())));
    }
    // Handle ::?CLASS — compile-time class pseudo-package
    if let Some(after) = rest.strip_prefix("?CLASS")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?CLASS".to_string())));
    }
    // Handle ::?ROLE — compile-time role pseudo-package
    if let Some(after) = rest.strip_prefix("?ROLE")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?ROLE".to_string())));
    }
    // Handle ::($expr) — indirect name lookup
    if let Some(after_paren) = rest.strip_prefix('(') {
        let (r, _) = ws(after_paren)?;
        let (r, inner) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, Expr::IndirectTypeLookup(Box::new(inner))));
    }
    let (rest, name) = crate::parser::stmt::ident_pub(rest)?;
    // Handle qualified names: ::Foo::Bar, with optional dynamic segments ::($expr)
    let mut full_name = name;
    let mut r = rest;
    let mut dynamic_expr: Option<Expr> = None;
    while r.starts_with("::") {
        let r2 = &r[2..];
        if let Some(after_paren) = r2.strip_prefix('(') {
            // Dynamic segment: ::($expr)
            let (r3, _) = ws(after_paren)?;
            let (r3, seg_expr) = expression(r3)?;
            let (r3, _) = ws(r3)?;
            let (r3, _) = parse_char(r3, ')')?;
            let base = if let Some(prev) = dynamic_expr.take() {
                Expr::Binary {
                    left: Box::new(prev),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(Expr::Literal(Value::str("::".to_string()))),
                }
            } else {
                Expr::Literal(Value::str(format!("{}::", full_name)))
            };
            dynamic_expr = Some(Expr::Binary {
                left: Box::new(base),
                op: crate::token_kind::TokenKind::Tilde,
                right: Box::new(seg_expr),
            });
            r = r3;
        } else if let Ok((r2, part)) = crate::parser::stmt::ident_pub(r2) {
            if let Some(ref mut dyn_expr) = dynamic_expr {
                // Append static segment to the dynamic expression
                let old = std::mem::replace(dyn_expr, Expr::Literal(Value::Nil));
                *dyn_expr = Expr::Binary {
                    left: Box::new(old),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(Expr::Literal(Value::str(format!("::{}", part)))),
                };
            } else {
                full_name = format!("{}::{}", full_name, part);
            }
            r = r2;
        } else {
            break;
        }
    }
    if let Some(dyn_expr) = dynamic_expr {
        return Ok((r, Expr::IndirectTypeLookup(Box::new(dyn_expr))));
    }
    // Type smileys: ::Foo:U, ::Foo:D, ::Foo:_
    if (r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_"))
        && !r[2..].starts_with('<')
    {
        let smiley = &r[..2];
        full_name = format!("{}{}", full_name, smiley);
        r = &r[2..];
    }
    Ok((r, Expr::BareWord(full_name)))
}

pub(crate) fn whatever(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '*')?;
    // ** is HyperWhatever (but not **N which is power op)
    if let Some(after) = input.strip_prefix('*') {
        // Make sure it's not *** or **N (power op with operand)
        if after.starts_with('*') {
            return Err(PError::expected("whatever"));
        }
        // **<digit> or **$ would be power op, but **) or **, or end is HyperWhatever
        if !after.is_empty()
            && !after.starts_with(')')
            && !after.starts_with(']')
            && !after.starts_with(',')
            && !after.starts_with(';')
            && !after.starts_with('}')
            && !after.starts_with(' ')
            && !after.starts_with('\n')
            && !after.starts_with('\t')
            && !after.starts_with('\r')
        {
            return Err(PError::expected("whatever (not **)"));
        }
        return Ok((after, Expr::HyperWhatever));
    }
    Ok((input, Expr::Whatever))
}

/// Parse keywords that are values: True, False, Nil, Any, Inf, NaN, etc.
pub(crate) fn keyword_literal(input: &str) -> PResult<'_, Expr> {
    // Try each keyword, ensuring it's not followed by alphanumeric (word boundary)
    // Also reject if followed by `(` to prevent treating e() as a constant
    let try_kw = |kw: &str, val: Value| -> PResult<'_, Expr> {
        // A user-declared type of the same name shadows the built-in term
        // constant (e.g. `class Empty {}` makes `Empty` refer to the class,
        // not the empty Slip); let it fall through to identifier parsing.
        // `Inf`/`NaN` are numeric literals in Raku and are NOT shadowable, so
        // they keep their constant value even when a like-named type exists.
        if !matches!(kw, "Inf" | "-Inf" | "NaN")
            && crate::parser::stmt::simple::is_user_declared_type(kw)
        {
            return Err(PError::expected("user-declared type shadows keyword"));
        }
        let (rest, _) = parse_tag(input, kw)?;
        // Check word boundary (superscript digits are NOT word chars)
        if let Some(c) = rest.chars().next()
            && (c.is_alphanumeric() || c == '_'
                // Hyphen is a word-continuation only for kebab-case identifiers,
                // i.e. when followed by an alphabetic char. `pi-1` is `pi` minus `1`.
                || (c == '-' && rest.chars().nth(1).is_some_and(|c2| c2.is_alphabetic())))
            && !is_superscript_digit(c)
        {
            return Err(PError::expected("word boundary"));
        }
        // Reject if followed by `(` - that's a function call, not a constant
        if rest.starts_with('(') {
            return Err(PError::expected("not a function call"));
        }
        // Reject if followed by `=>` (pair key context)
        let trimmed = rest.trim_start();
        if trimmed.starts_with("=>") && !trimmed.starts_with("==>") {
            return Err(PError::expected("not a pair key"));
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
    if let Ok(r) = try_kw("Empty", Value::Slip(std::sync::Arc::new(vec![]))) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Any", Value::Package(Symbol::intern("Any"))) {
        return Ok(r);
    }
    // Unicode: ∅ (U+2205 EMPTY SET)
    if input.starts_with('\u{2205}') {
        return Ok((
            &input['\u{2205}'.len_utf8()..],
            Expr::Literal(Value::set(std::collections::HashSet::new())),
        ));
    }
    if let Ok(r) = try_kw("Inf", Value::Num(f64::INFINITY)) {
        return Ok(r);
    }
    // ∞ (U+221E INFINITY)
    if input.starts_with('\u{221E}') {
        return Ok((
            &input['\u{221E}'.len_utf8()..],
            Expr::Literal(Value::Num(f64::INFINITY)),
        ));
    }
    if let Ok(r) = try_kw("-Inf", Value::Num(f64::NEG_INFINITY)) {
        return Ok(r);
    }
    // Note: -∞ is handled by prefix negation + ∞ literal, not as a special case,
    // so that -∞² parses correctly as -(∞²) not (-∞)².
    if let Ok(r) = try_kw("NaN", Value::Num(f64::NAN)) {
        return Ok(r);
    }
    // CORE-SETTING-REV — a compile-time constant equal to the language revision
    // letter (c/d/e) of the current compilation unit, as set by `use v6.X`.
    // The parser knows the active version while parsing each compunit, so we
    // fold it to a string literal at parse time (matching Rakudo, where it is a
    // BEGIN-time constant).
    {
        let ver = crate::parser::current_language_version();
        let rev = ver.rsplit('.').next().unwrap_or("c").to_string();
        if let Ok(r) = try_kw("CORE-SETTING-REV", Value::str(rev)) {
            return Ok(r);
        }
    }
    // rand — generates a random number (term)
    if input.starts_with("rand")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let after = input[4..].trim_start();
        // Don't treat as a call if followed by => (fat arrow creates a Pair)
        if after.starts_with("=>") && !after.starts_with("==>") {
            // Fall through to identifier_or_call which handles pair creation
        } else {
            // rand() and rand(N) are Perl 5 syntax — throw X::Obsolete
            let after_ws = after;
            if after_ws.starts_with('(') {
                return Err(PError::fatal(
                    "X::Obsolete: Unsupported use of rand().  In Raku please use: rand."
                        .to_string(),
                ));
            }
            return Ok((
                &input[4..],
                Expr::Call {
                    name: Symbol::intern("rand"),
                    args: vec![],
                },
            ));
        }
    }
    // now — returns current time as Instant (term)
    if input.starts_with("now")
        && !input[3..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let after = input[3..].trim_start();
        // Don't treat as a call if followed by => (fat arrow creates a Pair)
        if !after.starts_with("=>") || after.starts_with("==>") {
            return Ok((
                &input[3..],
                Expr::Call {
                    name: Symbol::intern("now"),
                    args: vec![],
                },
            ));
        }
    }
    // time — returns current epoch time as Int (term)
    if input.starts_with("time")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let after = input[4..].trim_start();
        // Don't treat as a call if followed by => (fat arrow creates a Pair)
        if !after.starts_with("=>") || after.starts_with("==>") {
            return Ok((
                &input[4..],
                Expr::Call {
                    name: Symbol::intern("time"),
                    args: vec![],
                },
            ));
        }
    }
    // times — returns ($user, $system) CPU times
    // Only treated as a 0-arg call when followed by parens or used standalone
    // (not when followed by => which creates a pair)
    if input.starts_with("times")
        && !input[5..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let after = input[5..].trim_start();
        // Don't treat as a call if followed by => (fat arrow creates a Pair)
        if !after.starts_with("=>") {
            return Ok((
                &input[5..],
                Expr::Call {
                    name: Symbol::intern("times"),
                    args: vec![],
                },
            ));
        }
    }
    // BEGIN/INIT/CHECK/END/ENTER/LEAVE as expression prefix phasers
    for (kw, kw_len, phaser_kind) in [
        ("BEGIN", 5, crate::ast::PhaserKind::Begin),
        ("INIT", 4, crate::ast::PhaserKind::Init),
        ("CHECK", 5, crate::ast::PhaserKind::Check),
        ("ENTER", 5, crate::ast::PhaserKind::Enter),
        ("LEAVE", 5, crate::ast::PhaserKind::Leave),
        ("END", 3, crate::ast::PhaserKind::End),
    ] {
        if input.starts_with(kw)
            && !input[kw_len..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
        {
            let r = &input[kw_len..];
            let (r, _) = ws(r)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::PhaserExpr {
                        kind: phaser_kind,
                        body,
                    },
                ));
            } else {
                let (r, expr) = expression_no_sequence(r)?;
                return Ok((
                    r,
                    Expr::PhaserExpr {
                        kind: phaser_kind,
                        body: vec![Stmt::Expr(expr)],
                    },
                ));
            }
        }
    }
    if input.starts_with("once")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let r = &input[4..];
        let (r, _) = ws(r)?;
        if r.starts_with('{') {
            let (r, body) = parse_block_body(r)?;
            return Ok((r, Expr::Once { body }));
        } else {
            let (r, expr) = expression_no_sequence(r)?;
            return Ok((
                r,
                Expr::Once {
                    body: vec![Stmt::Expr(expr)],
                },
            ));
        }
    }
    if let Ok(r) = try_kw("pi", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("π", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("tau", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("τ", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("e", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("\u{1D452}", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }

    Err(PError::expected("keyword literal"))
}
