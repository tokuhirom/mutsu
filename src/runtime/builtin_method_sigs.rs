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

/// What a core type declares under one method name.
#[derive(Default)]
struct Entry {
    /// An only method (a `-` row): its bind failure is not a NoMatch.
    only: bool,
    /// The proto's positional bounds, invocant included (a `#<arity>-<count>`
    /// row), when it does not take every positional (`proto method chars(*%)`).
    proto_positionals: Option<(usize, usize)>,
    /// The candidates' signatures, in dispatch order.
    sigs: Vec<&'static str>,
}

type Table = FxHashMap<&'static str, FxHashMap<&'static str, Entry>>;

static TSV: &str = include_str!("builtin_method_sigs.tsv");

// Cost: O(r), r = rows of the table, once per process.
fn table() -> &'static Table {
    static TABLE: OnceLock<Table> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table: Table = FxHashMap::default();
        for line in TSV.lines() {
            let mut parts = line.splitn(3, '\t');
            let (Some(ty), Some(method), Some(sig)) = (parts.next(), parts.next(), parts.next())
            else {
                continue;
            };
            let entry = table.entry(ty).or_default().entry(method).or_default();
            if sig == "-" {
                entry.only = true;
            } else if let Some(bounds) = sig.strip_prefix('#') {
                entry.proto_positionals = bounds
                    .split_once('-')
                    .and_then(|(lo, hi)| Some((lo.parse().ok()?, hi.parse().ok()?)));
            } else {
                entry.sigs.push(sig);
            }
        }
        table
    })
}

/// The entry of the first type in `type_name`'s ancestry (itself, then up to
/// `Any`/`Mu`) that declares `method`.
// Cost: O(a) hash probes, a = the ancestry length of `type_name`.
fn lookup(type_name: &str, method: &str) -> Option<&'static Entry> {
    let table = table();
    let own = [type_name];
    let chain = crate::builtin_types::ancestry::builtin_type_narrowness_chain(type_name)
        .unwrap_or(&own[..]);
    chain
        .iter()
        .chain(["Any", "Mu"].iter())
        .find_map(|ty| table.get(*ty)?.get(method))
}

/// Whether rakudo's core type `type_name` (or an ancestor up to `Any`/`Mu`)
/// declares `method`, as an only method or a multi.
// Cost: O(a) hash probes, a = the ancestry length of `type_name`.
pub(crate) fn declares(type_name: &str, method: &str) -> bool {
    lookup(type_name, method).is_some()
}

/// The candidate signatures `method` has on the core type `type_name`: the
/// list of the first type in its ancestry that declares the method. `None`
/// when that type declares it as an only method (its bind failure is not a
/// `X::Multi::NoMatch`), or when no listed type declares it (a type outside
/// the core catalog).
// Cost: O(a) hash probes, a = the ancestry length of `type_name`.
pub(crate) fn candidate_signatures(type_name: &str, method: &str) -> Option<&'static [&'static str]> {
    lookup(type_name, method)
        .filter(|entry| !entry.only)
        .map(|entry| entry.sigs.as_slice())
}

/// The candidate list of a `X::Multi::NoMatch` message for `method` on
/// `type_name`: one `\n    <signature>` line per candidate, empty when the
/// table lists none.
// Cost: O(a + c), a = the ancestry length of `type_name`, c = candidates.
pub(crate) fn candidate_lines(type_name: &str, method: &str) -> String {
    candidate_signatures(type_name, method)
        .map(|sigs| sigs.iter().map(|sig| format!("\n    {sig}")).collect())
        .unwrap_or_default()
}

/// The positional bounds (required, maximum; invocant included) of the proto
/// a multi `method` on `type_name` binds first, when it is bounded: a call
/// with more positionals fails the proto's bind ("Too many positionals
/// passed") rather than raising `X::Multi::NoMatch`.
// Cost: O(a) hash probes, a = the ancestry length of `type_name`.
pub(crate) fn proto_positional_bounds(type_name: &str, method: &str) -> Option<(usize, usize)> {
    lookup(type_name, method)
        .filter(|entry| !entry.only)
        .and_then(|entry| entry.proto_positionals)
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
    fn a_one_candidate_multi_is_listed() {
        let sigs = candidate_signatures("Any", "Array").expect("Any.Array is a multi");
        assert_eq!(sigs, ["($:: *%_)"]);
    }

    #[test]
    fn an_only_method_hides_an_ancestor_multi() {
        // `List.eager` is an only method; `Any.eager` is a multi.
        assert!(candidate_signatures("Any", "eager").is_some());
        assert!(candidate_signatures("List", "eager").is_none());
    }

    #[test]
    fn a_bounded_proto_is_recorded() {
        // `proto method chars(*%)` binds the invocant only.
        assert_eq!(super::proto_positional_bounds("Str", "chars"), Some((1, 1)));
        assert_eq!(super::proto_positional_bounds("Str", "lines"), None);
    }

    #[test]
    fn an_unknown_method_has_no_list() {
        assert!(candidate_signatures("Str", "no-such-method").is_none());
    }
}
