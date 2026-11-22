//! `Signature eqv Signature` and `Parameter eqv Parameter`, as Rakudo's
//! `infix:<eqv>` candidates in `Signature.rakumod` and `Parameter.rakumod`
//! define them: structural equivalence, which ignores a positional
//! parameter's name (`:(Int $x) eqv :(Int $y)`) but not a named one's.

use super::signature::{SigInfo, SigParam};

/// Whether two signatures are equivalent: the same return type, the same
/// number of parameters, the positionals pairwise equivalent and the nameds
/// equivalent by name, in any order.
// Cost: O(p^2) in the worst case, p = parameters (the named lookup is a
// scan; signatures are short).
pub(crate) fn signatures_eqv(a: &SigInfo, b: &SigInfo) -> bool {
    if normalized_type(a.return_type.as_deref()) != normalized_type(b.return_type.as_deref()) {
        return false;
    }
    params_eqv(&a.params, &b.params)
}

/// The parameter lists of two signatures (or sub-signatures).
// Cost: as `signatures_eqv`.
fn params_eqv(a: &[SigParam], b: &[SigParam]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    // The leading positionals pairwise; from the first mismatch on, every
    // remaining parameter must be named on both sides and match by name.
    let mut i = 0;
    while i < a.len() && param_eqv(&a[i], &b[i]) {
        i += 1;
    }
    if i == a.len() {
        return true;
    }
    if !a[i..].iter().all(|p| p.named) || !b[i..].iter().all(|p| p.named) {
        return false;
    }
    b[i..].iter().all(|pb| {
        a[i..]
            .iter()
            .find(|pa| first_name(pa) == first_name(pb))
            .is_some_and(|pa| param_eqv(pa, pb))
    })
}

/// Whether two parameters are equivalent: the same nominal type, the same
/// flags, equivalent sub-signatures, the same first name for a named one, and
/// the same literal post-constraint. A `where` block is a runtime check, so
/// it never matches.
// Cost: O(s), s = parameters of their sub-signatures.
fn param_eqv(a: &SigParam, b: &SigParam) -> bool {
    if normalized_type(a.type_constraint.as_deref()) != normalized_type(b.type_constraint.as_deref())
    {
        return false;
    }
    if flags(a) != flags(b) {
        return false;
    }
    let mut a_traits: Vec<&str> = a.traits.iter().map(String::as_str).collect();
    let mut b_traits: Vec<&str> = b.traits.iter().map(String::as_str).collect();
    a_traits.sort_unstable();
    b_traits.sort_unstable();
    if a_traits != b_traits {
        return false;
    }
    match (&a.sub_signature, &b.sub_signature) {
        (Some(sa), Some(sb)) if !params_eqv(sa, sb) => return false,
        (Some(_), None) | (None, Some(_)) => return false,
        _ => {}
    }
    if a.named && first_name(a) != first_name(b) {
        return false;
    }
    match (&a.literal_value, &b.literal_value) {
        (Some(la), Some(lb)) => la.eqv(lb),
        (None, None) => a.where_constraint.is_none() && b.where_constraint.is_none(),
        _ => false,
    }
}

/// The flags Rakudo's `$!flags` carries for a parameter.
// Cost: O(1).
fn flags(p: &SigParam) -> (char, bool, bool, bool, bool, bool, bool, bool, bool, bool, bool) {
    (
        p.sigil,
        p.named,
        p.slurpy,
        p.double_slurpy,
        p.onearg,
        p.is_capture,
        p.is_invocant,
        p.multi_invocant,
        p.sigilless,
        // Optional: a default, a `?` marker, or a named parameter that is not
        // `!`-required.
        !p.required || p.has_default || p.optional_marker,
        p.code_signature.is_some(),
    )
}

/// The first name of a named parameter (`:$a`, or the first of `:a(:$b)`).
// Cost: O(1).
fn first_name(p: &SigParam) -> &str {
    p.named_names
        .first()
        .map(String::as_str)
        .unwrap_or_else(|| p.name.trim_start_matches(['$', '@', '%', '&']))
}

/// A parameter's or return type's nominal type; an untyped one is `Any`.
// Cost: O(1).
fn normalized_type(ty: Option<&str>) -> &str {
    match ty {
        None | Some("") => "Any",
        Some(ty) => ty,
    }
}
