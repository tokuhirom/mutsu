use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;

/// Check for invalid type smileys (e.g. Int:foo) in a type constraint string.
/// Valid smileys are :D, :U, and :_. Anything else raises X::InvalidTypeSmiley.
pub(crate) fn check_invalid_type_smiley(type_constraint: &Option<String>) -> Result<(), PError> {
    if let Some(tc) = type_constraint
        && let Some(colon_pos) = tc.rfind(':')
        && (colon_pos == 0 || tc.as_bytes()[colon_pos - 1] != b':')
    {
        let after_colon = &tc[colon_pos + 1..];
        // Extract the smiley portion: only alphanumeric and underscore/hyphen chars
        let smiley_end = after_colon
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(after_colon.len());
        let smiley = &after_colon[..smiley_end];
        if !smiley.is_empty()
            && smiley != "D"
            && smiley != "U"
            && smiley != "_"
            && smiley.chars().next().is_some_and(|c| c.is_alphabetic())
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(smiley.to_string()));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                )),
            );
            let ex = Value::make_instance(Symbol::intern("X::InvalidTypeSmiley"), attrs);
            return Err(PError::fatal_with_exception(
                format!(
                    "Invalid type smiley ':{}' used, only ':D', ':U' and ':_' are allowed",
                    smiley
                ),
                Box::new(ex),
            ));
        }
    }
    Ok(())
}

pub(crate) fn parse_generic_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0usize;
    let mut end = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return Err(PError::expected("matching ']'"));
                }
                depth -= 1;
                if depth == 0 {
                    end = idx + ch.len_utf8();
                    break;
                }
            }
            _ => {}
        }
    }
    if end == 0 {
        return Err(PError::expected("matching ']'"));
    }
    Ok((&input[end..], input[..end].to_string()))
}

pub(crate) fn parse_type_constraint_expr(input: &str) -> Option<(&str, String)> {
    // Handle ::?CLASS and ::?ROLE pseudo-types
    let (mut rest, mut type_name) = if let Some(r) = input.strip_prefix("::?CLASS") {
        (r, "::?CLASS".to_string())
    } else if let Some(r) = input.strip_prefix("::?ROLE") {
        (r, "::?ROLE".to_string())
    // Handle type capture variables like `::a` (e.g., `my ::a $a`)
    } else if input.starts_with("::")
        && input[2..]
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
    {
        let r = &input[2..];
        let end = r
            .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(r.len());
        (&r[end..], format!("::{}", &r[..end]))
    } else {
        super::super::qualified_ident(input).ok()?
    };
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    } else if rest.starts_with(':')
        && rest[1..]
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
    {
        // Invalid type smiley like :foo — include it in the type name
        // so the caller can detect and report it as X::InvalidTypeSmiley
        let smiley_rest = &rest[1..];
        let end = smiley_rest
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(smiley_rest.len());
        type_name.push_str(&rest[..end + 1]);
        rest = &rest[end + 1..];
    }

    let (rest, type_name) = parse_of_type_constraint_chain(rest, type_name)?;

    let (r2, _) = ws(rest).ok()?;
    if let Some(after_open) = r2.strip_prefix('(') {
        let (after_ws, _) = ws(after_open).ok()?;
        if let Some(r3) = after_ws.strip_prefix(')') {
            let (r3, _) = ws(r3).ok()?;
            return Some((r3, format!("{}()", type_name)));
        }
        if let Some((inner_r, source_type)) = parse_type_constraint_expr(after_ws) {
            let (inner_r, _) = ws(inner_r).ok()?;
            if let Some(r3) = inner_r.strip_prefix(')') {
                let (r3, _) = ws(r3).ok()?;
                return Some((r3, format!("{}({})", type_name, source_type)));
            }
        }
    }
    Some((rest, type_name))
}

pub(crate) fn parse_of_type_constraint_chain(
    input: &str,
    base_type: String,
) -> Option<(&str, String)> {
    let mut rest = input;
    let mut type_name = base_type;
    loop {
        let (r_ws, _) = ws(rest).ok()?;
        let Some(after_of) = super::super::keyword("of", r_ws) else {
            break;
        };
        let (after_of, _) = ws(after_of).ok()?;
        let Some((r_elem, elem_type)) = parse_type_constraint_expr(after_of) else {
            break;
        };
        type_name = format!("{}[{}]", type_name, elem_type);
        rest = r_elem;
    }
    Some((rest, type_name))
}

pub(crate) fn parse_implicit_invocant_marker(input: &str) -> Option<(&str, String)> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
        || input.starts_with('*')
        || input.starts_with(':')
    {
        return None;
    }
    let (mut rest, mut type_name) = super::super::qualified_ident(input).ok()?;
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    }
    let after_colon = rest.strip_prefix(':')?;
    if after_colon.starts_with(':') {
        return None;
    }
    // If the character after ':' is alphanumeric or '_', this is likely a type
    // smiley (e.g., Int:foo) rather than an invocant marker (e.g., Int: $self).
    // Invocant markers must be followed by whitespace, sigil, or ')'.
    if after_colon
        .chars()
        .next()
        .is_some_and(|c| c.is_alphanumeric() || c == '_')
    {
        return None;
    }
    let (after_colon, _) = ws(after_colon).ok()?;
    Some((after_colon, type_name))
}
