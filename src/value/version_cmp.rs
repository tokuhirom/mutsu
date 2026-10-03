//! `Version` ordering. Pure, so it lives in `value` (#10779);
//! `runtime::utils` re-exports it.

pub(crate) fn version_cmp_parts(
    a_parts: &[crate::value::VersionPart],
    b_parts: &[crate::value::VersionPart],
) -> std::cmp::Ordering {
    use crate::value::VersionPart;
    let max_len = a_parts.len().max(b_parts.len());
    for i in 0..max_len {
        let a = a_parts.get(i);
        let b = b_parts.get(i);
        match (a, b) {
            (Some(VersionPart::Num(an)), Some(VersionPart::Num(bn))) => match an.cmp(bn) {
                std::cmp::Ordering::Equal => continue,
                other => return other,
            },
            (Some(VersionPart::Str(sa)), Some(VersionPart::Str(sb))) => match sa.cmp(sb) {
                std::cmp::Ordering::Equal => continue,
                other => return other,
            },
            // Str parts sort before Num parts (alpha/pre-release comes before release)
            (Some(VersionPart::Num(_)), Some(VersionPart::Str(_))) => {
                return std::cmp::Ordering::Greater;
            }
            (Some(VersionPart::Str(_)), Some(VersionPart::Num(_))) => {
                return std::cmp::Ordering::Less;
            }
            // Missing part defaults: Num(0) for missing
            (None, Some(VersionPart::Num(n))) => {
                if *n != 0 {
                    return std::cmp::Ordering::Less;
                }
            }
            (Some(VersionPart::Num(n)), None) => {
                if *n != 0 {
                    return std::cmp::Ordering::Greater;
                }
            }
            // Missing vs Str: missing (treated as Num(0)) is Greater than Str
            // (Str parts are pre-release, so they come before the plain version)
            (None, Some(VersionPart::Str(_))) => return std::cmp::Ordering::Greater,
            (Some(VersionPart::Str(_)), None) => return std::cmp::Ordering::Less,
            // A `*` (Whatever) part sorts *before* any concrete part (it acts as
            // -infinity for ordering: `v1.* <=> v1.0` is `Less`). This is distinct
            // from smart-matching, where a Whatever in the *matcher* accepts anything.
            (Some(VersionPart::Whatever), Some(VersionPart::Whatever)) => continue,
            (Some(VersionPart::Whatever), _) => return std::cmp::Ordering::Less,
            (_, Some(VersionPart::Whatever)) => return std::cmp::Ordering::Greater,
            (None, None) => continue,
        }
    }
    std::cmp::Ordering::Equal
}

/// Full `Version` ordering, including the trailing `+` / `-` flag as a
/// tie-breaker: when the parts compare equal, `v1+ <=> v1` is `More` (a `+`
/// version sorts *after* the bare version) and `-` sorts before it.
pub(crate) fn version_cmp(
    a_parts: &[crate::value::VersionPart],
    a_plus: bool,
    a_minus: bool,
    b_parts: &[crate::value::VersionPart],
    b_plus: bool,
    b_minus: bool,
) -> std::cmp::Ordering {
    match version_cmp_parts(a_parts, b_parts) {
        std::cmp::Ordering::Equal => {
            let a_rank = a_plus as i8 - a_minus as i8;
            let b_rank = b_plus as i8 - b_minus as i8;
            a_rank.cmp(&b_rank)
        }
        other => other,
    }
}
