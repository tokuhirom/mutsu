/// Identifier and pseudo-package parsing utilities.
///
/// These functions are used throughout the var-parsing submodules and by
/// external callers inside and outside `parser::primary`.
use crate::parser::helpers::{
    is_raku_identifier_continue, is_raku_identifier_start, normalize_raku_identifier,
};
use crate::parser::parse_result::{PError, PResult};

/// Parse identifier allowing kebab-case hyphens and apostrophes (e.g., `my-var`, `same'proto`).
/// A hyphen/apostrophe is only part of the name if followed by an alphabetic char or `_`,
/// so `$pd--` parses as `$pd` + postfix `--`.
pub(crate) fn parse_ident_with_hyphens<'a>(input: &'a str) -> PResult<'a, &'a str> {
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("identifier"))?;
    if !is_raku_identifier_start(first) && !first.is_numeric() {
        return Err(PError::expected("identifier"));
    }
    let mut end = first.len_utf8();
    for c in input[end..].chars() {
        if is_raku_identifier_continue(c) {
            end += c.len_utf8();
        } else {
            break;
        }
    }
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
                && is_raku_identifier_start(next)
            {
                end += sep.len_utf8();
                for c in after_sep.chars() {
                    if is_raku_identifier_continue(c) {
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

pub(crate) fn parse_qualified_ident_with_hyphens<'a>(input: &'a str) -> PResult<'a, String> {
    let (mut rest, first) = parse_ident_with_hyphens(input)?;
    let mut full = first.to_string();
    while let Some(after) = rest.strip_prefix("::") {
        let (r2, part) = parse_ident_with_hyphens(after)?;
        full.push_str("::");
        full.push_str(part);
        rest = r2;
    }
    Ok((rest, normalize_raku_identifier(&full)))
}

pub(crate) fn parse_qualified_ident_prefix_with_hyphens<'a>(input: &'a str) -> PResult<'a, String> {
    let (mut rest, first) = parse_ident_with_hyphens(input)?;
    let mut full = first.to_string();
    while let Some(after) = rest.strip_prefix("::") {
        if after.starts_with('(') {
            break;
        }
        if let Ok((r2, part)) = parse_ident_with_hyphens(after) {
            full.push_str("::");
            full.push_str(part);
            rest = r2;
        } else {
            break;
        }
    }
    Ok((rest, full))
}

pub(crate) fn parse_interpolation_qualified_ident_with_hyphens_or_empty(
    input: &str,
) -> (&str, String) {
    // Interpolation variable names should stop before Unicode superscript
    // exponent markers (e.g. "$no²" -> var "no", then literal "²").
    let mut rest = input;
    let mut full = String::new();
    let mut parsed_any = false;
    while let Some(first) = rest.chars().next() {
        if !(first.is_ascii_alphabetic() || first == '_') {
            break;
        }
        let mut end = first.len_utf8();
        let rest_bytes = rest.as_bytes();
        for c in rest[end..].chars() {
            if c.is_ascii_alphanumeric() || c == '_' {
                end += c.len_utf8();
            } else if c == '-' {
                // Kebab-case: hyphen is part of the identifier only when
                // followed by an alphabetic character (e.g. `$my-var`).
                // A trailing hyphen (`$a-`) is NOT part of the name.
                let next_pos = end + 1; // '-' is ASCII, always 1 byte
                if next_pos < rest_bytes.len() && rest_bytes[next_pos].is_ascii_alphabetic() {
                    end += c.len_utf8();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if parsed_any {
            full.push_str("::");
        }
        full.push_str(&rest[..end]);
        rest = &rest[end..];
        parsed_any = true;
        if let Some(after_scope) = rest.strip_prefix("::") {
            rest = after_scope;
        } else {
            break;
        }
    }
    if parsed_any {
        (rest, full)
    } else {
        (input, String::new())
    }
}

/// Check if an identifier is a known pseudo-package name.
pub(crate) fn is_pseudo_package(name: &str) -> bool {
    matches!(
        name,
        "SETTING"
            | "CALLER"
            | "CALLERS"
            | "OUTER"
            | "OUTERS"
            | "CORE"
            | "GLOBAL"
            | "MY"
            | "OUR"
            | "DYNAMIC"
            | "UNIT"
            | "LEXICAL"
            | "CLIENT"
            | "PROCESS"
            | "COMPILING"
    )
}
