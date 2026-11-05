//! The candidate signatures of rakudo's multi methods on core types, for the
//! candidate list of an `X::Multi::NoMatch` raised by a built-in method
//! ("Cannot resolve caller skip(Seq:D: Int:D, Int:D, Int:D); none of these
//! signatures matches:" followed by one line per candidate).
//!
//! Built-in methods are rows of the method table, not user-visible
//! `Signature`s, so the lists come from `builtin_method_sigs.tsv`, which
//! `scripts/builtin-method-sigs.raku` reads off rakudo's own multi
//! dispatchers (the same approach as `rakuast/ctor_names.tsv`).

use rustc_hash::FxHashMap;
use std::sync::OnceLock;

type Table = FxHashMap<&'static str, FxHashMap<&'static str, Vec<&'static str>>>;

static TSV: &str = include_str!("builtin_method_sigs.tsv");

// Cost: O(r), r = rows of the table, once per process.
fn table() -> &'static Table {
    static TABLE: OnceLock<Table> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table: Table = FxHashMap::default();
        for line in TSV.lines() {
            let mut parts = line.splitn(3, '\t');
            if let (Some(ty), Some(method), Some(sig)) = (parts.next(), parts.next(), parts.next())
            {
                table.entry(ty).or_default().entry(method).or_default().push(sig);
            }
        }
        table
    })
}

/// The candidate signatures `method` has on the core type `type_name`: the
/// list of the first type in its ancestry (itself, then up to `Any`/`Mu`)
/// whose multi dispatcher declares the method. `None` when no listed type
/// does (a single-candidate method, or a type outside the core catalog).
// Cost: O(a) hash probes, a = the ancestry length of `type_name`.
pub(crate) fn candidate_signatures(type_name: &str, method: &str) -> Option<&'static [&'static str]> {
    let table = table();
    let own = [type_name];
    let chain = crate::builtin_types::ancestry::builtin_type_narrowness_chain(type_name)
        .unwrap_or(&own[..]);
    chain
        .iter()
        .chain(["Any", "Mu"].iter())
        .find_map(|ty| table.get(*ty)?.get(method).map(Vec::as_slice))
}

#[cfg(test)]
mod tests {
    use super::candidate_signatures;

    #[test]
    fn a_method_declared_on_any_is_found_from_a_subtype() {
        let sigs = candidate_signatures("Seq", "skip").expect("Any.skip is a multi");
        assert_eq!(sigs.len(), 4);
        assert_eq!(sigs[0], "($:: *%_)");
    }

    #[test]
    fn a_str_method_lists_cool_and_str_candidates() {
        let sigs = candidate_signatures("Str", "lines").expect("Str.lines is a multi");
        assert_eq!(sigs.len(), 8);
        assert!(sigs[0].starts_with("(Cool:D"));
    }

    #[test]
    fn an_unknown_method_has_no_list() {
        assert!(candidate_signatures("Str", "no-such-method").is_none());
    }
}
