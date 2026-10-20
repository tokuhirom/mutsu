use super::term_literals::{
    can_name_type_smiley, is_fat_arrow_key, is_identifier_name, starts_with_type_smiley,
};
use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

pub(crate) fn declared_term_symbol(input: &str) -> PResult<'_, Expr> {
    if let Some((name, consumed_len, callable)) =
        crate::parser::stmt::simple::match_user_declared_term_symbol(input)
    {
        // An imported type may also be registered as a sigilless term. Let the
        // identifier parser consume its definiteness smiley as part of the
        // type name; otherwise `(CArray:D)` leaves `:D` after `CArray`.
        if !callable
            && starts_with_type_smiley(&input[consumed_len..])
            && can_name_type_smiley(&name)
        {
            return Err(PError::expected("definiteness-constrained type"));
        }
        if is_identifier_name(&input[..consumed_len]) && is_fat_arrow_key(&input[consumed_len..]) {
            return Err(PError::expected("declared term symbol"));
        }
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
        // A parenthesised `name(...)` was deferred above, so this bare name is
        // always the listop spelling.
        let expr = if callable {
            Expr::Call {
                name: Symbol::intern(&name),
                args: vec![],
                listop: true,
            }
        } else {
            Expr::BareWord(name)
        };
        return Ok((&input[consumed_len..], expr));
    }
    Err(PError::expected("declared term symbol"))
}

pub(crate) fn imported_value_term(input: &str) -> PResult<'_, Expr> {
    // A sigilless value term a `use`d module exports (`my \vrai = True;`
    // inside its `sub EXPORT` hook, or a `constant ... is export`) lives in a
    // SEPARATE registry (`imported_value_terms`) from a locally declared one
    // (`term_symbols`, checked earlier in primary dispatch) — see
    // `register_imported_value_term`'s doc. Without this alternative, an
    // imported term fell through to `identifier_or_call`'s general bareword
    // parsing, which has no "is this
    // a zero-arg term" exclusion of its own and defaults to a listop-call
    // head: French's `vrai et 2` misparsed as `vrai(et, 2)` and died
    // evaluating `et` as if it were a zero-arg call ("Unknown function: et").
    // As for local terms, a following `::` belongs to the qualified name:
    // importing a value `A` must not split `A::B` into two terms.
    if let Ok((rest, name)) = crate::parser::stmt::parse_raku_ident(input)
        && crate::parser::stmt::simple::is_imported_value_term(name)
        && !rest.starts_with("::")
        && !is_fat_arrow_key(rest)
        && !(starts_with_type_smiley(rest) && can_name_type_smiley(name))
        && !(rest.starts_with('(') && crate::parser::stmt::simple::is_imported_function(name))
    {
        return Ok((rest, Expr::BareWord(name.to_string())));
    }
    Err(PError::expected("declared term symbol"))
}
