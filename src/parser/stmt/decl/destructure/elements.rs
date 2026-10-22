//! Per-element parsing helpers for a grouped declaration
//! (`my ($a is rw, (\b, $c)) = ...`).

use super::super::super::super::helpers::{ws, ws1};
use super::super::super::super::parse_result::{PError, parse_char};
use super::super::super::{ident, keyword};
use super::super::helpers::register_term_symbol_from_decl_name;
use crate::ast::{ParamTrait, SignatureGroup, SignatureVar as DestructureVar};

/// Parse the `is rw` / `is raw` / `is copy` / `is readonly` traits a
/// declarator-list element may carry (`my ($a is rw, $b) := ...`). An unknown
/// trait is a compile-time error, as in a parameter declaration; `is rw` on an
/// `@`/`%` element is refused like rakudo does. Returns the input past the
/// traits (and trailing whitespace) plus the last trait seen.
pub(super) fn parse_element_traits<'a>(
    input: &'a str,
    name: &str,
) -> Result<(&'a str, Option<ParamTrait>), PError> {
    let mut r = input;
    let mut found = None;
    while let Some(after_is) = keyword("is", r) {
        let Ok((after_ws, _)) = ws1(after_is) else {
            break;
        };
        let (after_name, trait_name) = ident(after_ws)?;
        let t = match trait_name.as_str() {
            "rw" => ParamTrait::Rw,
            "raw" => ParamTrait::Raw,
            "copy" => ParamTrait::Copy,
            "readonly" => ParamTrait::Readonly,
            other => {
                return Err(PError::fatal(format!(
                    "Can't use unknown trait 'is' -> '{other}' in a parameter declaration."
                )));
            }
        };
        if t == ParamTrait::Rw && name.starts_with(['@', '%']) {
            let sigil = &name[..1];
            return Err(PError::fatal(format!(
                "For parameter '{name}', '{sigil}' sigil containers don't need 'is rw' to be writable\n\
Can only use 'is rw' on a scalar ('$' sigil) parameter, not '{name}'"
            )));
        }
        found = Some(t);
        let (after, _) = ws(after_name)?;
        r = after;
    }
    Ok((r, found))
}

/// Collect the flattened targets and the source nesting of a declaration group.
// Cost: O(n), n = targets and delimiters, with nesting bounded at 128.
pub(super) fn collect_nested_group_vars<'a>(
    input: &'a str,
    vars: &mut Vec<DestructureVar>,
    depth: usize,
) -> Result<(&'a str, SignatureGroup), PError> {
    if depth > 128 {
        return Err(PError::fatal(
            "nested declaration group exceeds 128 levels".to_string(),
        ));
    }
    let (mut r, _) = parse_char(input, '(')?;
    let (r2, _) = ws(r)?;
    r = r2;
    let mut members = Vec::new();
    loop {
        if r.starts_with(')') {
            break;
        }
        if r.starts_with('(') {
            let (rest, group) = collect_nested_group_vars(r, vars, depth + 1)?;
            members.push(group);
            r = rest;
        } else if let Some(after_backslash) = r.strip_prefix('\\') {
            let (r2, name) = ident(after_backslash)?;
            register_term_symbol_from_decl_name(&name);
            vars.push(DestructureVar {
                name,
                is_slurpy: false,
                is_optional: false,
                is_named: false,
                default: None,
                per_var_type_constraint: None,
                where_constraint: None,
                sigilless: true,
                literal_value: None,
                param_trait: None,
            });
            members.push(SignatureGroup::Var(vars.len() - 1));
            r = r2;
        } else {
            let sigil = r.as_bytes().first().copied().unwrap_or(0);
            if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
                let prefix = match sigil {
                    b'@' => "@",
                    b'%' => "%",
                    b'&' => "&",
                    _ => "",
                };
                let (r2, n) = crate::parser::stmt::lexical_var_name(r)?;
                vars.push(DestructureVar {
                    name: format!("{}{}", prefix, n),
                    is_slurpy: false,
                    is_optional: false,
                    is_named: false,
                    default: None,
                    per_var_type_constraint: None,
                    where_constraint: None,
                    sigilless: false,
                    literal_value: None,
                    param_trait: None,
                });
                members.push(SignatureGroup::Var(vars.len() - 1));
                r = r2;
            } else {
                return Err(PError::expected(
                    "variable sigil ($, @, %, &) or sigilless (\\name) in nested destructure group",
                ));
            }
        }
        let (r2, _) = ws(r)?;
        r = r2;
        if r.starts_with(',') {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            r = r2;
        }
    }
    let (r, _) = parse_char(r, ')')?;
    Ok((r, SignatureGroup::Group(members)))
}
