//! `Signature ~~ Signature`: whether one signature accepts every call another does.

use super::signature::{SigInfo, SigParam};

/// Answers "is the type named `t2` the type named `t1` or a subtype of it?" for
/// the names the builtin type catalog behind [`is_supertype_of`] does not know — user
/// classes and roles, which need the runtime's registry.
pub(crate) type UserTypeCheck<'a> = dyn FnMut(&str, &str) -> bool + 'a;

/// Signature-Signature smartmatch: $s2 ~~ $s1
/// Returns true if $s1 ACCEPTS $s2, meaning $s1 is at least as general as $s2.
pub(crate) fn signature_smartmatch(s1: &SigInfo, s2: &SigInfo) -> bool {
    signature_smartmatch_with(s1, s2, &mut |_, _| false)
}

/// [`signature_smartmatch`] with a registry-aware fallback for user types, so
/// `:(Derived $) ~~ :(Base $)` holds when `Derived` is a user class inheriting
/// (or doing) `Base` — Rakudo's `Parameter.ACCEPTS` is `$!type.ACCEPTS(other.type)`.
// Cost: O(p * c), p = parameter count, c = cost of one `user_type` query.
pub(crate) fn signature_smartmatch_with(
    s1: &SigInfo,
    s2: &SigInfo,
    user_type: &mut UserTypeCheck<'_>,
) -> bool {
    if (s1.return_type.is_some() || s2.return_type.is_some()) && s1.return_type != s2.return_type {
        return false;
    }

    // Slurpy hash (*%_) belongs to the named space even without the : prefix
    let is_named_space = |p: &&SigParam| p.named || (p.slurpy && p.sigil == '%');
    let s1_positional: Vec<&SigParam> = s1.params.iter().filter(|p| !is_named_space(p)).collect();
    let s1_named: Vec<&SigParam> = s1.params.iter().filter(|p| is_named_space(p)).collect();
    let s2_positional: Vec<&SigParam> = s2.params.iter().filter(|p| !is_named_space(p)).collect();
    let s2_named: Vec<&SigParam> = s2.params.iter().filter(|p| is_named_space(p)).collect();

    if s1_positional.iter().any(|p| p.is_capture) {
        return true;
    }

    let s1_has_slurpy_hash = s1_named.iter().any(|p| p.slurpy);
    let s1_has_slurpy_array = s1_positional.iter().any(|p| p.slurpy);

    let s1_pos_required: Vec<&&SigParam> = s1_positional
        .iter()
        .filter(|p| !p.slurpy && !p.is_optional())
        .collect();
    let s1_pos_all: Vec<&&SigParam> = s1_positional.iter().filter(|p| !p.slurpy).collect();
    let s2_pos_required: Vec<&&SigParam> = s2_positional
        .iter()
        .filter(|p| !p.slurpy && !p.is_optional())
        .collect();
    let s2_pos_all: Vec<&&SigParam> = s2_positional.iter().filter(|p| !p.slurpy).collect();

    if s2_positional.iter().any(|p| p.is_capture) && !s1_positional.iter().any(|p| p.is_capture) {
        return false;
    }

    if s2_positional.iter().any(|p| p.slurpy && p.sigil == '@')
        && !s1_has_slurpy_array
        && !s1_positional.iter().any(|p| p.is_capture)
    {
        return false;
    }

    if !s1_has_slurpy_array {
        if s2_pos_required.len() > s1_pos_all.len() {
            return false;
        }
        if s1_pos_required.len() > s2_pos_required.len() {
            return false;
        }
        if s2_pos_all.len() > s1_pos_all.len() {
            return false;
        }
    }

    let check_count = s1_pos_all.len().min(s2_pos_all.len());
    for i in 0..check_count {
        let p1 = s1_pos_all[i];
        let p2 = s2_pos_all[i];
        if !type_accepts(
            p1.type_constraint.as_deref(),
            p2.type_constraint.as_deref(),
            user_type,
        ) {
            return false;
        }
        if let (Some(sub1), Some(sub2)) = (&p1.sub_signature, &p2.sub_signature) {
            let sub_info1 = SigInfo {
                params: sub1.clone(),
                return_type: None,
            };
            let sub_info2 = SigInfo {
                params: sub2.clone(),
                return_type: None,
            };
            if !signature_smartmatch_with(&sub_info1, &sub_info2, user_type) {
                return false;
            }
        }
        if p1.sub_signature.is_some() && p2.sub_signature.is_none() {
            return false;
        }
        if let (Some(cs1), Some(cs2)) = (&p1.code_signature, &p2.code_signature)
            && !signature_smartmatch_with(cs1, cs2, user_type)
        {
            return false;
        }
        if p1.sigil == '@' && p2.sigil == '$' {
            return false;
        }
        // Compare literal values: if s1 (RHS/accepting) has a literal,
        // s2 (LHS/topic) must have the same literal value.
        if let Some(ref lit1) = p1.literal_value {
            match &p2.literal_value {
                Some(lit2) => {
                    if !lit1.eqv(lit2) {
                        return false;
                    }
                }
                None => {
                    // s1 has a literal but s2 doesn't — s2 is more general,
                    // so s1 does not accept s2
                    return false;
                }
            }
        }
    }

    for i in 0..check_count {
        let p1 = s1_pos_all[i];
        let p2 = s2_pos_all[i];
        if p2.is_optional() && !p1.is_optional() && !p1.slurpy {
            return false;
        }
    }

    let s1_named_nonslurpy: Vec<&&SigParam> = s1_named.iter().filter(|p| !p.slurpy).collect();
    let s2_named_nonslurpy: Vec<&&SigParam> = s2_named.iter().filter(|p| !p.slurpy).collect();
    let s2_has_slurpy_hash = s2_named.iter().any(|p| p.slurpy);

    for p2 in &s2_named_nonslurpy {
        if s1_has_slurpy_hash {
            continue;
        }
        let found = s1_named_nonslurpy.iter().any(|p1| p1.name == p2.name);
        if !found {
            return false;
        }
    }

    if s2_has_slurpy_hash && !s1_has_slurpy_hash && !s1_positional.iter().any(|p| p.is_capture) {
        return false;
    }

    for p1 in &s1_named_nonslurpy {
        if p1.required {
            let found = s2_named_nonslurpy.iter().any(|p2| p2.name == p1.name);
            if !found && !s2_has_slurpy_hash {
                return false;
            }
        }
    }

    for p1 in &s1_named_nonslurpy {
        if let Some(p2) = s2_named_nonslurpy.iter().find(|p| p.name == p1.name) {
            if p2.required && !p1.required && !s1_has_slurpy_hash {
                return false;
            }
            if p1.required && !p2.required {
                return false;
            }
            // Compare type constraints for named params
            if !type_accepts(
                p1.type_constraint.as_deref(),
                p2.type_constraint.as_deref(),
                user_type,
            ) {
                return false;
            }
            // Compare outer sub-signatures for named params (e.g., :x($r) (Str $g, Any $i))
            if let (Some(os1), Some(os2)) = (&p1.outer_sub_signature, &p2.outer_sub_signature) {
                let sub_info1 = SigInfo {
                    params: os1.clone(),
                    return_type: None,
                };
                let sub_info2 = SigInfo {
                    params: os2.clone(),
                    return_type: None,
                };
                if !signature_smartmatch_with(&sub_info1, &sub_info2, user_type) {
                    return false;
                }
            }
            if p1.outer_sub_signature.is_some() && p2.outer_sub_signature.is_none() {
                return false;
            }
        } else if s2_has_slurpy_hash {
            // s2 doesn't have this named param — it relies on slurpy hash.
            // If s1 constrains this param's type, s1 is more restrictive than s2's catch-all.
            if !type_accepts(p1.type_constraint.as_deref(), None, user_type) {
                return false;
            }
        }
    }

    true
}

fn type_accepts(
    type1: Option<&str>,
    type2: Option<&str>,
    user_type: &mut UserTypeCheck<'_>,
) -> bool {
    match (type1, type2) {
        (None, _) => true,                             // s1 untyped (Any) accepts anything
        (Some(t1), None) => is_supertype_of(t1, "Mu"), // s1 typed, s2 untyped (Any) → s1 must accept Any
        (Some(t1), Some(t2)) => is_supertype_of(t1, t2) || user_type(t1, t2),
    }
}

/// Whether the builtin type `t2` is `t1` or narrower, read from the builtin
/// type catalog, the one ancestry oracle (ADR-0051 P2): a class in `t2`'s MRO
/// or a role it composes (`Pair ~~ Associative`, `Array ~~ Positional`).
/// User classes and roles are answered by the caller's `user_type` fallback.
// Cost: O(m + r), m = MRO length of `t2`, r = total roles along it.
fn is_supertype_of(t1: &str, t2: &str) -> bool {
    match t1 {
        _ if t1 == t2 => true,
        "Mu" => true,
        "Any" => t2 != "Mu" && t2 != "Junction",
        _ => crate::builtin_types::ancestry::builtin_type_is_a(t2, t1),
    }
}
