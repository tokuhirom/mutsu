//! ADR-0019 Phase E, box E2a: recognition metadata for native method dispatch.
//!
//! `native_method_0arg`/`native_method_1arg`/`native_method_2arg`
//! (`builtins/methods_0arg/`, `builtins/methods_narg/`) decide by matching on the
//! method-name string whether a call is served by the pure native layer. Phase
//! E's resolver needs to ask the same question ("does (owner, name) admit a
//! native call, and at which arity?") WITHOUT invoking the cascade -- that is
//! recognition metadata, not invocation, and it is what [`NativeMethodRow`]
//! records. See `todo/deep/adr0019-e2-e4-resolver-core.md` decision 1.
//!
//! Invocation stays in the arity cascades until Phase F retires them; nothing
//! in the VM or the interpreter's real dispatch reads a row today. E2a's rows
//! are a deliberately conservative first pass, generated once (2026-08-10) by
//! probing every (owner, name) pair from the existing 14 built-in-method name
//! slices ([`super::builtin_type_methods`]) against the real native dispatch
//! cascades with a representative sample value per owner, plus a type-object
//! probe for the [`TYPE_OBJECT_OK`](NativeRowFlags::TYPE_OBJECT_OK) flag. A
//! pair recognized at NO arity is conservatively classified `N`/`SPECIAL` (or
//! `MUTATES_RECEIVER` when the name also appears in the Tier-A mutable-method
//! dispatch, `vm/vm_call_method_mut_ops.rs`) -- exactly mirroring what
//! `native_call_unmodeled` (`vm/vm_stats.rs`) would flag as unrecognized if a
//! real call reached that (owner, name) pair. Per the doc comment on
//! `builtin_type_methods`, row *generation* must stay static (no native-method
//! invocation during real `Interpreter` construction); the probing that
//! produced [`RAW_ROWS`] below ran once in a throwaway `#[test]`, and its
//! output was pasted here as plain data -- production code never calls a
//! native method to build this table.
//!
//! E2b drives the gap between this conservative table and the cascades'
//! actual behavior to zero (see the design doc's classification table and the
//! `native_call_unmodeled` counter this box wires up).

#[cfg(test)]
use super::builtin_type_methods::builtin_method_entries;
use std::collections::HashMap;
use std::sync::OnceLock;

/// Bitmask of the call arities a native row may be served at. `N` covers both
/// "no pure-arity cascade recognizes this name" (needs a slow path / `&mut
/// self`) and 3+-arg hand-rolled pre-dispatch escapes -- E2a does not need to
/// tell those apart, since neither is invoked through the arity cascades a
/// [`NativeArityMask`] describes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NativeArityMask(pub(crate) u8);

impl NativeArityMask {
    pub(crate) const A0: NativeArityMask = NativeArityMask(1 << 0);
    pub(crate) const A1: NativeArityMask = NativeArityMask(1 << 1);
    pub(crate) const A2: NativeArityMask = NativeArityMask(1 << 2);
    /// Slow-path / special: not served by `native_method_{0,1,2}arg` at all.
    pub(crate) const N: NativeArityMask = NativeArityMask(1 << 3);

    pub(crate) const fn contains(self, bit: NativeArityMask) -> bool {
        self.0 & bit.0 != 0
    }

    /// The single-arity mask a `native_method_{0,1,2}arg` call of this arity
    /// falls under; `N` for anything the arity cascades never take directly.
    pub(crate) const fn for_arity(arity: usize) -> NativeArityMask {
        match arity {
            0 => Self::A0,
            1 => Self::A1,
            2 => Self::A2,
            _ => Self::N,
        }
    }
}

/// Method-identity facts a resolver needs about a native row, independent of
/// any particular receiver's runtime state (see the design doc's admission-gate
/// classification table, decision 3).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NativeRowFlags(pub(crate) u8);

impl NativeRowFlags {
    /// Callable on a type object (`Str:U`), not just a defined instance.
    /// Only consumed by the E2a inverse probe today; E4 reads it once the
    /// resolver admits type-object receivers to a candidate sequence.
    #[cfg(test)]
    pub(crate) const TYPE_OBJECT_OK: NativeRowFlags = NativeRowFlags(1 << 0);
    /// Implemented by a Tier-A mutable-method helper (`vm_call_method_mut_ops.rs`)
    /// or a `&mut self` slow path, not the pure `native_method_*arg` layer.
    /// Only consumed by the E2a inverse probe today; E6 is the real reader.
    #[cfg(test)]
    pub(crate) const MUTATES_RECEIVER: NativeRowFlags = NativeRowFlags(1 << 1);
    /// Handled by a named interceptor ahead of the arity cascades (or not
    /// natively recognized at all) -- never resolved by plain
    /// `native_method_*arg` name matching.
    pub(crate) const SPECIAL: NativeRowFlags = NativeRowFlags(1 << 2);

    #[cfg(test)]
    pub(crate) const fn contains(self, bit: NativeRowFlags) -> bool {
        self.0 & bit.0 != 0
    }
}

/// One canonical native (owner, method) recognition entry -- see the module
/// doc comment. Mirrors [`super::builtin_type_methods::BuiltinMethodEntry`]'s
/// `owner`/`name`/`order` and adds the arity/flags recognition metadata.
/// `#[cfg(test)]`: E2a's only reader is the inverse probe below;
/// [`native_method_row`] (the point lookup `record_native_row_coverage` uses)
/// is the production entry point until E4a builds a resolver sequence from
/// the full per-owner catalog.
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct NativeMethodRow {
    pub(crate) owner: &'static str,
    pub(crate) name: &'static str,
    pub(crate) order: u16,
    pub(crate) arity: NativeArityMask,
    pub(crate) flags: NativeRowFlags,
}

/// `(owner, name, arity_bits, flag_bits)`, generated 2026-08-10 -- see the
/// module doc comment for how. Not hand-maintained; regenerate by re-running
/// the probe (see the ADR PR for the generating snippet) rather than
/// hand-editing rows in place, the same discipline `builtin_type_methods.rs`
/// uses for its name lists.
use super::native_method_row_table::RAW_ROWS;

type RowKey = (&'static str, &'static str);
type RowValue = (u8, u8);

fn classification_table() -> &'static HashMap<RowKey, RowValue> {
    static TABLE: OnceLock<HashMap<RowKey, RowValue>> = OnceLock::new();
    TABLE.get_or_init(|| {
        RAW_ROWS
            .iter()
            .map(|&(owner, name, arity, flags)| ((owner, name), (arity, flags)))
            .collect()
    })
}

/// The recognition row for one `(owner, name)` pair. A pair with no entry in
/// [`RAW_ROWS`] -- an owner E2a's probe did not cover (`Sub`/`Signature`/
/// `IO::Path`/`IO::Handle`/`Cool`), the untouched majority of `Any`/`Mu`'s own
/// method surface (E2b added only `so`/`not`/`defined`/`DEFINITE` by hand so
/// far), or a name the probe itself did not recognize at any arity --
/// conservatively reports `N`/`SPECIAL`: "not
/// servable by the pure arity cascades", never a false claim of coverage.
pub(crate) fn native_method_row(
    owner: &'static str,
    name: &'static str,
) -> (NativeArityMask, NativeRowFlags) {
    classification_table()
        .get(&(owner, name))
        .map(|&(arity, flags)| (NativeArityMask(arity), NativeRowFlags(flags)))
        .unwrap_or((NativeArityMask::N, NativeRowFlags::SPECIAL))
}

/// The full native-method-row catalog for one built-in owner, in `.^methods`
/// catalog order -- the [`NativeMethodRow`] counterpart of
/// [`builtin_method_entries`]. `#[cfg(test)]`: see [`NativeMethodRow`]'s doc.
#[cfg(test)]
pub(crate) fn native_method_rows(type_name: &str) -> Vec<NativeMethodRow> {
    builtin_method_entries(type_name)
        .into_iter()
        .map(|entry| {
            let (arity, flags) = native_method_row(entry.owner, entry.name);
            NativeMethodRow {
                owner: entry.owner,
                name: entry.name,
                order: entry.order,
                arity,
                flags,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::builtin_type_methods::{builtin_sample_value, native_method_arities};
    use crate::symbol::Symbol;
    use crate::value::Value;

    #[test]
    fn raw_rows_have_no_duplicate_keys() {
        let mut seen = std::collections::HashSet::new();
        for &(owner, name, _, _) in RAW_ROWS {
            assert!(
                seen.insert((owner, name)),
                "duplicate row for {owner}x{name}"
            );
        }
    }

    #[test]
    fn unmodelled_pair_is_conservatively_special() {
        let (arity, flags) = native_method_row("NoSuchOwner", "no-such-method");
        assert_eq!(arity, NativeArityMask::N);
        assert!(flags.contains(NativeRowFlags::SPECIAL));
    }

    /// ADR-0019 E2b: the hand-added `Any`/`Mu` universal rows (`so`, `not`,
    /// `defined`, `DEFINITE`) are not tied to one probed owner's sample value
    /// -- unlike the per-type rows above, they claim to be recognized by the
    /// shared arity-0 cascade arms (`dispatch_core_str`/`dispatch_core_coerce`)
    /// for EVERY receiver, which is exactly why the coverage check must walk
    /// the dispatch chain to find them (`Interpreter::record_native_row_coverage`).
    /// Verify that claim directly against two structurally different sample
    /// receivers.
    #[test]
    fn any_mu_universal_rows_are_backed_by_the_cascade_for_multiple_receiver_types() {
        let str_sample = builtin_sample_value("Str").unwrap();
        let int_sample = builtin_sample_value("Int").unwrap();
        for name in ["so", "not", "defined"] {
            let (arity, _flags) = native_method_row("Any", name);
            assert!(
                arity.contains(NativeArityMask::A0),
                "{name} row should claim A0"
            );
            for sample in [&str_sample, &int_sample] {
                assert!(
                    native_method_arities(sample, name) & 1 != 0,
                    "{name} should be recognized at arity 0 for {sample:?}"
                );
            }
        }
        let (definite_arity, definite_flags) = native_method_row("Mu", "DEFINITE");
        assert!(definite_arity.contains(NativeArityMask::A0));
        assert!(definite_flags.contains(NativeRowFlags::SPECIAL));
        for sample in [&str_sample, &int_sample] {
            assert!(native_method_arities(sample, "DEFINITE") & 1 != 0);
        }
    }

    #[test]
    fn native_method_rows_matches_builtin_entry_count() {
        for owner in ["Str", "Int", "List", "Array", "Hash"] {
            assert_eq!(
                native_method_rows(owner).len(),
                builtin_method_entries(owner).len()
            );
        }
    }

    /// The inverse probe from the design doc (decision 2): for every row that
    /// claims a pure arity bit and is not flagged `SPECIAL`/`MUTATES_RECEIVER`,
    /// actually call the corresponding cascade with a representative receiver
    /// and require it to recognize the name at that arity. This is what would
    /// catch a row over-claiming coverage the cascade does not provide -- the
    /// reverted 2026-08-04 attempt's exact failure mode.
    #[test]
    fn inverse_probe_pure_arity_rows_are_backed_by_the_cascade() {
        let probed_owners = [
            "Str", "Int", "Num", "Rat", "Complex", "List", "Array", "Hash", "Bool", "Range",
        ];
        for owner in probed_owners {
            let sample = builtin_sample_value(owner).expect("sample value for probed owner");
            for row in native_method_rows(owner) {
                if row.flags.contains(NativeRowFlags::SPECIAL)
                    || row.flags.contains(NativeRowFlags::MUTATES_RECEIVER)
                {
                    continue;
                }
                let observed = native_method_arities(&sample, row.name);
                for (bit, mask) in [
                    (0u8, NativeArityMask::A0),
                    (1u8, NativeArityMask::A1),
                    (2u8, NativeArityMask::A2),
                ] {
                    if row.arity.contains(mask) {
                        assert!(
                            observed & (1 << bit) != 0,
                            "{owner}x{}row claims arity {bit} but the cascade does not recognize it on the sample value",
                            row.name
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn type_object_ok_rows_are_backed_by_a_real_type_object_probe() {
        let probed_owners = ["Str", "Int", "Bool", "Array", "List", "Hash", "Range"];
        for owner in probed_owners {
            let type_obj = Value::package(Symbol::intern(owner));
            for row in native_method_rows(owner) {
                if row.flags.contains(NativeRowFlags::TYPE_OBJECT_OK) {
                    let observed = native_method_arities(&type_obj, row.name);
                    assert_ne!(
                        observed, 0,
                        "{owner}x{}claims TYPE_OBJECT_OK but no arity recognizes a type object",
                        row.name
                    );
                }
            }
        }
    }

    /// ADR-0019 E2b: the hand-added `Pair`/`Seq` rows below were generated by
    /// probing a curated candidate-name list (drawn from the `Seq`/`Pair` type
    /// docs plus the `native_call_unmodeled` sweep breakdown) against a real
    /// `Value::pair`/`Value::seq` sample, the same "call the real cascade
    /// once, paste the result as static data" discipline the module doc
    /// describes for the original 11 owners -- neither `Pair` nor `Seq` has a
    /// `builtin_type_method_names` entry to draw candidates from instead (see
    /// `native_method_row_table.rs`'s comment on those two owners). This test
    /// is the inverse-probe half of that discipline for the two owners: every
    /// non-`SPECIAL`/`MUTATES_RECEIVER` row claiming a pure arity bit must be
    /// backed by the real cascade for a representative sample, catching a row
    /// that over-claims coverage.
    /// ADR-0019 E2b: the hand-added `Match` rows were generated by running a
    /// real regex match through the interpreter and probing every candidate
    /// name against the resulting `Match` value (see
    /// `native_method_row_table.rs`'s comment on the `Match` rows for why
    /// candidates include the full `Str` name list, not just Match's own
    /// documented methods). This is the inverse-probe half of that
    /// discipline: every non-`SPECIAL`/`MUTATES_RECEIVER` `Match` row
    /// claiming a pure arity bit must be backed by the real cascade for a
    /// representative Match sample.
    #[test]
    fn match_rows_are_backed_by_the_cascade() {
        let mut interp = crate::runtime::Interpreter::new();
        interp.run("'foo' ~~ /f(o)(o)/;").unwrap();
        let sample = interp.env().get("/").cloned().unwrap();
        for &(row_owner, name, arity, flags) in RAW_ROWS {
            if row_owner != "Match" {
                continue;
            }
            let flags = NativeRowFlags(flags);
            if flags.contains(NativeRowFlags::SPECIAL)
                || flags.contains(NativeRowFlags::MUTATES_RECEIVER)
            {
                continue;
            }
            let observed = native_method_arities(&sample, name);
            let mask = NativeArityMask(arity);
            for (bit, m) in [
                (0u8, NativeArityMask::A0),
                (1u8, NativeArityMask::A1),
                (2u8, NativeArityMask::A2),
            ] {
                if mask.contains(m) {
                    assert!(
                        observed & (1 << bit) != 0,
                        "Match x {name} row claims arity {bit} but the cascade does not recognize it"
                    );
                }
            }
        }
    }

    /// The `Match` rows deliberately omit `so`/`not`/`defined`: a Match's
    /// `dispatch_owner_chain` includes `Any`, so those three are already
    /// covered by the `Any` universal rows via the chain-walk. Confirm that
    /// premise directly, since it is what justifies leaving them out of the
    /// `Match`-owner rows above (mirroring `Pair`/`Seq`).
    #[test]
    fn match_so_not_defined_are_covered_via_the_any_chain() {
        let mut interp = crate::runtime::Interpreter::new();
        interp.run("'foo' ~~ /f(o)(o)/;").unwrap();
        let sample = interp.env().get("/").cloned().unwrap();
        let chain = interp.dispatch_owner_chain(&sample);
        assert!(
            chain.iter().any(|t| t.as_str() == "Any"),
            "Match's dispatch_owner_chain should include Any: {chain:?}"
        );
        for name in ["so", "not", "defined"] {
            assert!(
                native_method_row("Match", name).0 == NativeArityMask::N,
                "{name} should not have its own Match row (covered via Any)"
            );
            assert!(
                native_method_arities(&sample, name) & 1 != 0,
                "{name} should be recognized at arity 0 for a Match sample"
            );
        }
    }

    #[test]
    fn pair_seq_rows_are_backed_by_the_cascade() {
        let pair = Value::pair("k".to_string(), Value::int(1));
        let seq = Value::seq(vec![Value::int(1), Value::int(2), Value::int(3)]);
        for (owner, sample) in [("Pair", &pair), ("Seq", &seq)] {
            for &(row_owner, name, arity, flags) in super::super::native_method_row_table::RAW_ROWS
            {
                if row_owner != owner {
                    continue;
                }
                let flags = NativeRowFlags(flags);
                if flags.contains(NativeRowFlags::SPECIAL)
                    || flags.contains(NativeRowFlags::MUTATES_RECEIVER)
                {
                    continue;
                }
                let observed = native_method_arities(sample, name);
                let mask = NativeArityMask(arity);
                for (bit, m) in [
                    (0u8, NativeArityMask::A0),
                    (1u8, NativeArityMask::A1),
                    (2u8, NativeArityMask::A2),
                ] {
                    if mask.contains(m) {
                        assert!(
                            observed & (1 << bit) != 0,
                            "{owner}x{name} row claims arity {bit} but the cascade does not recognize it"
                        );
                    }
                }
            }
        }
    }

    /// ADR-0019 E2b: the hand-added `List`/`Array` extra rows (names absent
    /// from `LIST_METHODS`) were generated by probing a curated candidate
    /// list against real `Value::array` samples (see
    /// `native_method_row_table.rs`'s comment on the `List`/`Array` extra
    /// rows). This is the inverse-probe half of that discipline.
    #[test]
    fn array_list_extra_rows_are_backed_by_the_cascade() {
        use crate::builtins::builtin_type_methods::builtin_sample_value;
        let array = builtin_sample_value("Array").unwrap();
        let list = builtin_sample_value("List").unwrap();
        for (owner, sample) in [("Array", &array), ("List", &list)] {
            for &(row_owner, name, arity, flags) in RAW_ROWS {
                if row_owner != owner {
                    continue;
                }
                let flags = NativeRowFlags(flags);
                if flags.contains(NativeRowFlags::SPECIAL)
                    || flags.contains(NativeRowFlags::MUTATES_RECEIVER)
                {
                    continue;
                }
                let observed = native_method_arities(sample, name);
                let mask = NativeArityMask(arity);
                for (bit, m) in [
                    (0u8, NativeArityMask::A0),
                    (1u8, NativeArityMask::A1),
                    (2u8, NativeArityMask::A2),
                ] {
                    if mask.contains(m) {
                        assert!(
                            observed & (1 << bit) != 0,
                            "{owner}x{name} row claims arity {bit} but the cascade does not recognize it"
                        );
                    }
                }
            }
        }
    }

    /// `.dynamic` is deliberately `Array`-only, not `List` -- the cascade's
    /// own guard in `methods_0arg/mod.rs` restricts it to non-`List`-kind
    /// Array values. Confirm the row addition did not accidentally claim it
    /// for `List` too.
    #[test]
    fn list_dynamic_is_not_recognized() {
        use crate::builtins::builtin_type_methods::builtin_sample_value;
        let list = builtin_sample_value("List").unwrap();
        assert_eq!(native_method_arities(&list, "dynamic"), 0);
        assert_eq!(native_method_row("List", "dynamic").0, NativeArityMask::N);
    }
}
