//! Identifier, keyword, and variable-name parsing helpers.
//!
//! Moved out of `stmt/mod.rs` (§7-8 cohesive split). These functions parse
//! Raku identifiers, qualified names, keywords, and variable names. They are
//! re-exported from `mod.rs` at their original visibility so external callers
//! (`stmt::<name>` / `super::<name>`) keep working unchanged.

use super::*;

/// Try to match a keyword at the start of input, ensuring word boundary.
pub(crate) fn keyword<'a>(kw: &str, input: &'a str) -> Option<&'a str> {
    if input.starts_with(kw) && !is_ident_char(input.as_bytes().get(kw.len()).copied()) {
        Some(&input[kw.len()..])
    } else {
        None
    }
}

/// True when `input` begins a `unit class`/`unit role`/`unit grammar`
/// declaration (the semicolon, rest-of-unit form), whose following statements
/// must be captured as the type's body. `unit module`/`unit package` are
/// deliberately excluded — their package context is set compiler-side instead.
pub(crate) fn starts_unit_class_role_grammar(input: &str) -> bool {
    let Some(r) = keyword("unit", input) else {
        return false;
    };
    let Ok((r, _)) = ws1(r) else {
        return false;
    };
    keyword("class", r).is_some() || keyword("role", r).is_some() || keyword("grammar", r).is_some()
}

/// Parse an identifier (alphanumeric, _, -).
/// Hyphen is only allowed when followed by an alphabetic char (not a digit).
pub(crate) fn ident(input: &str) -> PResult<'_, String> {
    let (rest, name) = parse_raku_ident(input)?;
    Ok((rest, normalize_raku_identifier(name)))
}

/// Parse a Raku-style identifier.
/// Allows hyphens and apostrophes between word segments, but only when followed by a letter.
/// e.g. `foo-bar` is valid, `doesn't` is valid, but `foo-3` stops at `foo`.
pub(crate) fn parse_raku_ident<'a>(input: &'a str) -> PResult<'a, &'a str> {
    // Check first character: underscore or Unicode alphabetic character.
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("identifier"))?;
    if !is_raku_identifier_start(first) {
        return Err(PError::expected("identifier"));
    }
    let mut end = first.len_utf8();
    // Continue consuming identifier characters
    let mut chars = input[end..].chars().peekable();
    while let Some(&c) = chars.peek() {
        if is_raku_identifier_continue(c) {
            end += c.len_utf8();
            chars.next();
        } else if c == '-' || c == '\'' {
            // Hyphen/apostrophe is part of identifier only if followed by a letter
            let mut lookahead = chars.clone();
            lookahead.next(); // skip the hyphen/apostrophe
            if let Some(&next) = lookahead.peek() {
                if is_raku_identifier_start(next) {
                    end += 1; // consume the hyphen/apostrophe
                    chars.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((&input[end..], &input[..end]))
}

/// Parse a qualified identifier (Foo::Bar::Baz).
/// `X::Syntax::Name::Null` — a qualified name with an empty component, e.g.
/// `Foo::::Bar` or `$a::::b`.
fn name_null_error() -> PError {
    let msg = "Name component may not be null".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
    let ex = crate::value::Value::make_instance(
        crate::symbol::Symbol::intern("X::Syntax::Name::Null"),
        attrs,
    );
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn qualified_ident(input: &str) -> PResult<'_, String> {
    let (mut rest, name) = ident(input)?;
    let mut full = name;
    while rest.starts_with("::") {
        let r = &rest[2..];
        // `Foo::::Bar` / `$a::::b` — an empty component between `::` separators
        // is a null name component, which Raku rejects at compile time.
        if r.starts_with("::") {
            return Err(name_null_error());
        }
        // Handle ::<SYMBOL> subscript syntax (e.g., CORE::<&run>)
        if let Some(after_bracket) = r.strip_prefix('<')
            && let Some(end) = after_bracket.find('>')
        {
            let symbol = &after_bracket[..end];
            full.push_str("::");
            full.push_str(symbol);
            rest = &after_bracket[end + 1..];
            continue;
        }
        if let Ok((r2, part)) = ident(r) {
            full.push_str("::");
            full.push_str(&part);
            rest = r2;
        } else {
            return Err(PError::expected_at("identifier after '::'", r));
        }
    }
    Ok((rest, full))
}

/// Parse a variable name ($x, @arr, %hash) and return just the name part.
pub(crate) fn var_name(input: &str) -> PResult<'_, String> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
    {
        let r = &input[1..];
        // Detect Perl 5 special variables before parsing twigils.
        if input.starts_with('$')
            && let Some(err) = crate::parser::primary::var::detect_perl5_scalar_var(r)
        {
            return Err(PError::fatal(err));
        }
        // Handle twigils
        let (r, twigil) =
            if r.starts_with('*') || r.starts_with('?') || r.starts_with('!') || r.starts_with('^')
            {
                (&r[1..], &r[..1])
            } else {
                (r, "")
            };
        // Handle $_ special
        if input.starts_with('$')
            && r.starts_with('_')
            && (r.len() == 1
                || !r
                    .as_bytes()
                    .get(1)
                    .is_some_and(|c| c.is_ascii_alphanumeric()))
        {
            return Ok((&r[1..], "_".to_string()));
        }
        // Handle $/ special
        if input.starts_with('$') && r.starts_with('/') {
            return Ok((&r[1..], "/".to_string()));
        }
        // Handle callable operator references: &infix:<...>, &prefix:<...>, ...
        if input.starts_with('&') {
            for op_kind in ["infix", "prefix", "postfix", "term", "circumfix"] {
                if let Some(after_kind) = r.strip_prefix(op_kind)
                    && let Some(after_colon) = after_kind.strip_prefix(':')
                {
                    if let Some(after_open) = after_colon.strip_prefix("<<")
                        && let Some(end) = after_open.find(">>")
                    {
                        let symbol = &after_open[..end];
                        return Ok((&after_open[end + 2..], format!("{op_kind}:<{symbol}>")));
                    }
                    if let Some(after_open) = after_colon.strip_prefix('<')
                        && let Some(end) = after_open.find('>')
                    {
                        let symbol = &after_open[..end];
                        return Ok((&after_open[end + 1..], format!("{op_kind}:<{symbol}>")));
                    }
                }
            }
        }
        // Handle $! as a special variable (not twigil + name)
        if input.starts_with('$') && twigil == "!" {
            // If no identifier follows, this is the $! error variable
            if r.is_empty() || !r.chars().next().is_some_and(is_raku_identifier_start) {
                return Ok((r, "!".to_string()));
            }
        }
        if twigil.is_empty()
            && let Some(after_bracket) = r.strip_prefix("::<")
            && let Some(end) = after_bracket.find('>')
        {
            let symbol = &after_bracket[..end];
            if !symbol.is_empty() {
                return Ok((&after_bracket[end + 1..], format!("::<{symbol}>")));
            }
        }
        // A fatal qualified-name error (e.g. a null component `$a::::b`) must
        // propagate, not fall through to the `$foo::` / anonymous-variable arms.
        if let Err(e) = qualified_ident(r)
            && e.is_fatal()
        {
            return Err(e);
        }
        // Handle bare $ (anonymous variable) — no name after sigil
        if let Ok((mut rest, mut name)) = qualified_ident(r) {
            while rest.starts_with(':') && !rest.starts_with("::") {
                let after_colon = &rest[1..];
                if let Ok((r2, suffix)) = ident(after_colon) {
                    name.push(':');
                    name.push_str(&suffix);
                    rest = r2;
                    // Double colon after adverb is X::Syntax::Confused
                    if rest.starts_with("::") {
                        return Err(PError::fatal(
                            "X::Syntax::Confused: Confused (double colon in variable adverb)"
                                .to_string(),
                        ));
                    }
                    // Parse adverb value: <...>, «...», [...], (...)
                    if let Some((canonical, r3)) =
                        crate::parser::primary::var::parse_adverb_value_pub(rest)
                    {
                        name.push_str(&canonical);
                        rest = r3;
                    }
                } else {
                    break;
                }
            }
            let full = if twigil.is_empty() {
                name
            } else {
                format!("{}{}", twigil, name)
            };
            Ok((rest, full))
        } else if let Ok((after_ident, ident_name)) = ident(r)
            && after_ident.starts_with("::")
            && twigil.is_empty()
        {
            // Variable with trailing :: (e.g., `$foo::`) — package-stash variable.
            // In Raku, `my $foo::` declares a variable named `$foo::` (distinct from `$foo`).
            let rest = &after_ident[2..];
            Ok((rest, format!("{}::", ident_name)))
        } else if input.starts_with('$') && twigil.is_empty() {
            // Anonymous scalar variable: bare $
            Ok((r, "__ANON_STATE__".to_string()))
        } else if input.starts_with('@') && twigil.is_empty() {
            Ok((r, "__ANON_ARRAY__".to_string()))
        } else if input.starts_with('%') && twigil.is_empty() {
            Ok((r, "__ANON_HASH__".to_string()))
        } else if input.starts_with('&') && twigil.is_empty() {
            Ok((r, "__ANON_CODE__".to_string()))
        } else {
            Err(PError::expected("variable name"))
        }
    } else {
        Err(PError::expected("variable name"))
    }
}
