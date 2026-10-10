//! Export tags on routine declarations.
use super::*;

pub(crate) fn parse_export_trait_tags(input: &str) -> PResult<'_, Vec<String>> {
    let mut tags = Vec::new();
    let (mut rest, _) = ws(input)?;
    if !rest.starts_with('(') {
        return Ok((rest, tags));
    }

    let after_open = &rest[1..];
    let mut depth = 1usize;
    let mut end: Option<usize> = None;
    for (i, ch) in after_open.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ')' in export trait"))?;
    let inner = &after_open[..end];
    rest = &after_open[end + 1..];

    let mut i = 0usize;
    while i < inner.len() {
        let c = inner[i..].chars().next().unwrap_or('\0');
        let c_len = c.len_utf8();
        if c.is_whitespace() || c == ',' {
            i += c_len;
            continue;
        }
        if c == ':' {
            i += c_len;
            if let Some(next) = inner[i..].chars().next()
                && next == '!'
            {
                i += next.len_utf8();
            }
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            if i > start {
                let tag = inner[start..i].to_string();
                if !tags.iter().any(|t| t == &tag) {
                    tags.push(tag);
                }
            }
            continue;
        }
        // A bare identifier (no `:` adverb prefix) inside `export(...)` is a term
        // reference, not an export tag (`export(:FOO)` is the tag form). An
        // undeclared bare name there is X::Undeclared::Symbols, matching rakudo
        // ("Undeclared name: WTF").
        if c.is_alphabetic() || c == '_' {
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == ':' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            let name = &inner[start..i];
            return Err(PError::fatal(format!(
                "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                name
            )));
        }
        i += c_len;
    }

    let (rest, _) = ws(rest)?;
    Ok((rest, tags))
}
