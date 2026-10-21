//! Anonymous regex declarators retain signatures, trees and written source.
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::value::Value;

/// The optional `<signature>` of an anonymous `token`/`regex`/`rule` term
/// (`token ( Str:D $text! where { … } ) { … }`). Mirrors the named
/// declarator's signature parse in `grammar_module::token_decl`; the caller
/// only commits to the declarator reading once a `{` body follows, so a
/// parse failure here simply falls through to the ordinary term handling.
pub(super) fn parse_anon_regex_signature(input: &str) -> PResult<'_, Vec<crate::ast::ParamDef>> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, param_defs) = crate::parser::stmt::parse_param_list_pub(r)?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    Ok((r, param_defs))
}

/// The execution pattern of an anonymous `token`/`regex`/`rule` term: the
/// body normalized the way a named declarator's is, then given that
/// declarator's implicit `rule` whitespace and ratchet semantics.
/// The value of an anonymous `regex`/`token`/`rule` term. `body_region` is the
/// input at the body's `{` and `after` what follows its `}`, which locate the
/// body in the unit; the value carries the source tree (with the declarator
/// kind) so the RakuAST frontend can render it as a declaration.
pub(super) fn anon_regex_value(
    declaration_region: &str,
    body_region: &str,
    after: &str,
    raw_body: &str,
    kind: crate::regex_tree::RegexDeclKind,
    params: Option<Vec<crate::ast::ParamDef>>,
) -> Result<Value, PError> {
    use crate::parser::stmt::class::token_body::normalize_token_pattern;
    let body_source = source_prefix(body_region, after.len()).ok_or_else(|| {
        PError::fatal("Anonymous regex body lost its source boundary".to_string())
    })?;
    let declaration_source = source_prefix(declaration_region, after.len()).ok_or_else(|| {
        PError::fatal("Anonymous regex declaration lost its source boundary".to_string())
    })?;
    let source = normalize_token_pattern(raw_body);
    let unit_base =
        crate::parser::primary::fragment_attempts::unit_offset_of_copy(body_source, &source);
    let tree =
        crate::regex_tree::RegexTree::parse_static_at(&source, true, unit_base).map(|mut tree| {
            tree.declaration_kind = Some(kind);
            tree
        });
    let value = Value::anon_regex_code(finalize_anon_regex_pattern(raw_body, kind), params);
    let value = match tree {
        Some(tree) => value.with_regex_source_tree(tree),
        None => value,
    };
    Ok(value.with_regex_declared_source(declaration_source))
}

fn finalize_anon_regex_pattern(body: &str, kind: crate::regex_tree::RegexDeclKind) -> String {
    use crate::parser::stmt::class::token_body::{
        finalize_anon_declarator_pattern, normalize_token_pattern,
    };
    finalize_anon_declarator_pattern(&normalize_token_pattern(body), kind)
}

// Cost: O(1), UTF-8 source boundaries are checked before slicing.
fn source_prefix(region: &str, after_len: usize) -> Option<&str> {
    region
        .len()
        .checked_sub(after_len)
        .and_then(|end| region.get(..end))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn invalid_source_boundaries_are_parse_errors() {
        for (declaration, body, after) in [
            ("token {", "{", "too long"),
            ("token {\u{e9}}", "{\u{e9}}", "xx"),
        ] {
            assert!(
                anon_regex_value(
                    declaration,
                    body,
                    after,
                    "",
                    crate::regex_tree::RegexDeclKind::Token,
                    None
                )
                .is_err()
            );
        }
        assert_eq!(source_prefix("\u{e9}}", 1), Some("\u{e9}"));
    }
}
