//! Trait-clause parsing for `enum` declarations (`is export`, `does Role`),
//! split out of `enum_decl.rs` to keep that file under the 500-line limit.
//! See `parse_enum_trait_clauses` and `skip_trailing_enum_does_clause` for
//! why the BEFORE-the-value-list and AFTER-the-value-list spellings of
//! `does` need genuinely different handling (#8216).

use super::super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::super::parse_result::{PError, PResult};
use super::super::{ident, keyword, qualified_ident};
use super::enum_decl::skip_balanced_brackets;
use super::helpers::{has_export_tag_argument, parse_export_trait_tags};

/// Parse a run of the declaration's trait clauses -- `is <trait>` (e.g. `is
/// export`) and `does <Role>` -- BEFORE the enum's value list, which may
/// appear in any order and repeat (`enum E does A does B is export <x y>`).
/// Without the `does` arm at all, the clause was left unconsumed, the
/// `(...)`/`<...>` body was never read as the enum's value list, and the
/// leftover `does Role (A => 1, B => 2)` parsed as a plain expression
/// statement -- which is where the spurious "Useless use of '=>' in sink
/// context" warning came from, and why the enum ended up with no values at
/// all.
///
/// A `does` clause AFTER the value list is a different grammar position
/// entirely and does NOT reach this function -- see
/// [`skip_trailing_enum_does_clause`].
pub(super) fn parse_enum_trait_clauses<'a>(
    input: &'a str,
    is_export: &mut bool,
    export_tags: &mut Vec<String>,
    roles: &mut Vec<String>,
) -> Result<&'a str, PError> {
    let mut rest = input;
    loop {
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            let (r, trait_name) = ident(r)?;
            if trait_name == "export" {
                *is_export = true;
                let (r, tags) = if has_export_tag_argument(r) {
                    parse_export_trait_tags(r)?
                } else {
                    (r, Vec::new())
                };
                if tags.is_empty() {
                    if !export_tags.iter().any(|t| t == "DEFAULT") {
                        export_tags.push("DEFAULT".to_string());
                    }
                } else {
                    for tag in tags {
                        if !export_tags.iter().any(|t| t == &tag) {
                            export_tags.push(tag);
                        }
                    }
                }
                let (r, _) = ws(r)?;
                rest = r;
                continue;
            }
            // Consume an optional parenthesized argument for other traits.
            let r = skip_balanced_parens(r);
            let (r, _) = ws(r)?;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("does", rest) {
            let (r, _) = ws1(r)?;
            // An anonymous role LITERAL (`does role { ... }`) is invalid
            // here, exactly as rakudo rejects it ("Invalid typename
            // 'role'") -- unlike the AFTER-the-value-list spelling
            // (`enum X <values> does role {...}`, see
            // `skip_trailing_enum_does_clause`), this BEFORE-the-values
            // `does` is a genuine declarator trait, and rakudo's trait slot
            // only ever accepts a role NAME there, never a literal. Falling
            // through to `qualified_ident` below (unchanged) reads `role`
            // as an ordinary name and reports "Unknown role: role" at
            // composition time -- a different error text than rakudo's, but
            // the same rejection, not a silent mis-composition (#8216).
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
    Ok(rest)
}

/// Consume (and discard) a single trailing `does <Role>` clause after an
/// enum's value list (#8216). Unlike the `does`/`is` trait clauses BEFORE
/// the value list -- genuine declarator traits that really compose the role
/// (verified against rakudo: `.^does` is `True` there) -- `enum X <values>
/// does Role` is not special enum grammar at all: `enum X <values>` parses
/// as an ordinary TERM (the enum's type object) in this position, and the
/// trailing `does Role` is the general infix `does` operator (the same one
/// `$x does role {...}` uses to mix a role into a COPY of `$x`), applied to
/// that term and then sunk with no lasting effect (verified against
/// rakudo: `.^does`/`.^roles(:local)` stay `False`/empty for this ordering,
/// and a SECOND trailing `does` errors "non-associative, requires
/// parentheses" -- a real infix operator's fixity, not a repeatable
/// declarator trait).
///
/// Mirroring that (an inert, single, non-repeating consumption) avoids two
/// real bugs a naive "just don't error" fix would still have: leaving `does
/// role { ... }` unconsumed crashed on `role` as an unknown role NAME looked
/// up on the ENCLOSING scope (`Unknown role: role`, #8216's original
/// report), and leaving its `{ ... }` body unconsumed split it into a
/// stray, unrelated bare-block statement.
pub(super) fn skip_trailing_enum_does_clause(input: &str) -> PResult<'_, ()> {
    let Some(r) = keyword("does", input) else {
        return Ok((input, ()));
    };
    let (r, _) = ws1(r)?;
    if keyword("role", r).is_some() {
        let (r, _role_expr) = crate::parser::primary::misc::anon_role_expr(r)?;
        let (r, _) = ws(r)?;
        return Ok((r, ()));
    }
    let (r, _role_name) = qualified_ident(r)?;
    let r = skip_balanced_brackets(r).unwrap_or(r);
    let (r, _) = ws(r)?;
    Ok((r, ()))
}
