//! Return-trait type spelling.
use super::*;

/// Parse the type named by `returns` / `of` / `-->`, including a coercion type's
/// parenthesized source: `Str`, `Str()`, `Int(Str)`. Without this, `returns Str()`
/// stopped at the `(` and the trailing `()` was left to be parsed as a sub body.
pub(super) fn parse_trait_type_name(input: &str) -> PResult<'_, String> {
    // `::?CLASS` / `::?ROLE` pseudo-types (the current class/role) may appear as a
    // return type (`method m() of ::?CLASS`). The generic identifier scan below
    // stops at `?`, leaving a stray `?CLASS`, so handle them up front. An optional
    // definedness smiley (`::?CLASS:D`) is folded in.
    for pseudo in ["::?CLASS", "::?ROLE"] {
        if let Some(after) = input.strip_prefix(pseudo) {
            let (rest, name) =
                if after.starts_with(":D") || after.starts_with(":U") || after.starts_with(":_") {
                    (&after[2..], format!("{}{}", pseudo, &after[..2]))
                } else {
                    (after, pseudo.to_string())
                };
            return Ok((rest, name));
        }
    }
    // Return types use the same identifier segments as declarations.  In
    // particular, a qualified type may have a hyphenated final segment
    // (`LibCurl::version-info`), which is common in NativeCall wrappers.
    let (rest, base) = take_while1(input, |c: char| {
        c.is_alphanumeric() || matches!(c, '_' | ':' | '-' | '\'')
    })?;
    let mut base = base.to_string();
    let mut rest = rest;
    // Parametrization: `returns Array[Int]`, `of Maybe[Array]`. Scan a balanced
    // `[...]` (nested brackets allowed) and fold it into the type-name string,
    // matching how the `-->` return-type annotation records `"Array[Int]"`.
    if rest.starts_with('[') {
        let bytes = rest.as_bytes();
        let mut depth = 0i32;
        let mut end = None;
        for (i, &b) in bytes.iter().enumerate() {
            match b {
                b'[' => depth += 1,
                b']' => {
                    depth -= 1;
                    if depth == 0 {
                        end = Some(i);
                        break;
                    }
                }
                _ => {}
            }
        }
        let Some(close) = end else {
            return Err(PError::expected("closing ']' of a parametrized type"));
        };
        base.push_str(&rest[..=close]);
        rest = &rest[close + 1..];
    }
    let Some(inner) = rest.strip_prefix('(') else {
        return Ok((rest, base));
    };
    let mut depth = 1usize;
    for (idx, ch) in inner.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    // An empty source is `Any`: `Str()` is `Str(Any)`, which is what
                    // `.returns` reports.
                    let source = inner[..idx].trim();
                    let source = if source.is_empty() { "Any" } else { source };
                    return Ok((&inner[idx + 1..], format!("{base}({source})")));
                }
            }
            _ => {}
        }
    }
    Err(PError::expected("closing ')' of a coercion type"))
}
