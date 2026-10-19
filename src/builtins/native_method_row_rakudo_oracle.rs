//! The row catalog's `DECLARED` / `INTROSPECTABLE` flags checked against
//! Rakudo itself (#11271).
//!
//! `.^can` reads `DECLARED` and `.^methods` reads `INTROSPECTABLE` for every
//! built-in owner, so a row that is missing (or missing the bit) makes
//! introspection deny a method mutsu dispatches fine: before this check,
//! `Rat.^can('numerator')`, `Num.^can('isNaN')` and `Int.^can('FatRat')` were
//! all `False`. The bits were baked once from Rakudo and nothing kept them in
//! step with methods added to the native cascades afterwards.
//!
//! The oracle is `rakudo_method_tables.txt`, the committed output of
//! `scripts/gen-rakudo-method-tables.raku`: per owner, the keys of
//! `::(Owner).^method_table` (`declared`) and the names of
//! `::(Owner).^methods` (`methods`), and the names language revision 6.e
//! adds to them (`declared.e`, `methods.e`), which a `REVISION_E` row is
//! checked against. Two directions are checked:
//!
//! - **no false claim**: a `DECLARED` / `INTROSPECTABLE` bit on a name Rakudo
//!   does not declare / list for that owner;
//! - **no denial**: a name Rakudo declares / lists for an owner, which the
//!   native cascades recognize on a sample value of that owner, but whose row
//!   lacks the bit. Only owners with a [`builtin_sample_value`] can be probed.
//!
//! A failure prints the row literals to paste into
//! `native_method_row_table.rs`.
//!
//! The no-denial check and the `DECLARED` false-claim check are `#[ignore]`d
//! and off the CI gate (#11405): coupling every cascade change to a
//! hand-edited shared row kept `main` red on 2026-10-03. The
//! `INTROSPECTABLE` false-claim check is always run: adding a native method
//! cannot fail it. Run the ignored checks on demand with
//! `cargo test --lib native_method_row_rakudo_oracle -- --ignored`.

use super::builtin_type_methods::canonical_builtin_owner;
use super::native_method_row::NativeRowFlags;
use super::native_method_row_table::RAW_ROWS;
use std::collections::{BTreeMap, BTreeSet};

const SNAPSHOT: &str = include_str!("rakudo_method_tables.txt");

/// `kind -> owner -> names`, `kind` being `declared` or `methods`.
fn rakudo_tables() -> BTreeMap<&'static str, BTreeMap<&'static str, BTreeSet<&'static str>>> {
    let mut tables: BTreeMap<_, BTreeMap<_, BTreeSet<_>>> = BTreeMap::new();
    for line in SNAPSHOT.lines().filter(|l| !l.starts_with('#')) {
        let mut fields = line.split('\t');
        let (Some(kind), Some(owner)) = (fields.next(), fields.next()) else {
            continue;
        };
        tables
            .entry(kind)
            .or_default()
            .insert(owner, fields.collect());
    }
    tables
}

/// Whether language revision 6.e adds `name` to `owner`'s method table: the
/// `declared.e` lines a `REVISION_E` method row is checked against.
pub(crate) fn declared_in_revision_e(owner: &str, name: &str) -> bool {
    rakudo_tables()
        .get("declared.e")
        .and_then(|owners| owners.get(owner))
        .is_some_and(|names| names.contains(name))
}

/// The snapshot list a row is checked against: `kind`, or for a
/// [`NativeRowFlags::REVISION_E`] row the names language revision 6.e adds
/// (`declared.e`, `methods.e`, `only.e`; ADR-11276 §9.146).
fn list_kind(kind: &'static str, flags: NativeRowFlags) -> &'static str {
    if !flags.contains(NativeRowFlags::REVISION_E) {
        return kind;
    }
    match kind {
        "declared" => "declared.e",
        "methods" => "methods.e",
        _ => "only.e",
    }
}

/// `.^methods` is served from the row catalog only for an owner that is its
/// own canonical owner (`builtin_type_methods::builtin_type_method_names`);
/// every other owner's `.^methods` comes from elsewhere, so its
/// `INTROSPECTABLE` bits are not read and not checked here.
fn introspection_reads_rows(owner: &str) -> bool {
    canonical_builtin_owner(owner) == owner
}

#[test]
fn rakudo_snapshot_parses() {
    let tables = rakudo_tables();
    assert!(tables["declared"].contains_key("Str"));
    assert!(tables["declared"]["Rat"].contains("numerator"));
    assert!(tables["methods"]["Int"].contains("FatRat"));
    assert!(tables["only"]["Str"].contains("Int"));
    assert!(!tables["only"]["Str"].contains("uc"));
}

/// #10234: `ONLY_METHOD` makes an `augment` that redeclares the name a
/// compile-time error, so a false claim rejects a legal program. This
/// direction stays on the gate; it can only fail on a row that sets the bit.
#[test]
fn only_method_bits_are_never_false_claims() {
    let tables = rakudo_tables();
    let mut wrong = Vec::new();
    for &(owner, name, _, flags) in RAW_ROWS {
        let flags = NativeRowFlags(flags);
        if flags.contains(NativeRowFlags::ONLY_METHOD)
            && (!flags.contains(NativeRowFlags::DECLARED)
                || !tables[list_kind("only", flags)]
                    .get(owner)
                    .is_some_and(|only| only.contains(name)))
        {
            wrong.push(format!("{owner}.{name}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "ONLY_METHOD set on a name Rakudo does not declare as an only method:\n{}",
        wrong.join("\n")
    );
}

/// The other direction: a `DECLARED` row whose Rakudo method is an `only`
/// one but lacks `ONLY_METHOD` lets an `augment` silently replace it. Off the
/// gate for the same reason as the checks below (#11405).
#[test]
#[ignore = "off the CI gate until the check is rebuilt: #11405"]
fn only_methods_are_never_unmarked() {
    let tables = rakudo_tables();
    let mut missing = Vec::new();
    for &(owner, name, arity, flags) in RAW_ROWS {
        let have = NativeRowFlags(flags);
        if have.contains(NativeRowFlags::DECLARED)
            && !have.contains(NativeRowFlags::ONLY_METHOD)
            && tables["only"]
                .get(owner)
                .is_some_and(|only| only.contains(name))
        {
            missing.push(format!(
                "    (\"{owner}\", \"{name}\", {arity}, {}),",
                flags | NativeRowFlags::ONLY_METHOD.0
            ));
        }
    }
    assert!(
        missing.is_empty(),
        "{} DECLARED row(s) lack ONLY_METHOD:\n{}",
        missing.len(),
        missing.join("\n")
    );
}

/// `DECLARED` false claims remain an advisory check until #11405 is resolved.
#[test]
#[ignore = "off the CI gate until the check is rebuilt: #11405"]
fn declared_bits_are_never_false_claims() {
    let tables = rakudo_tables();
    let mut wrong = Vec::new();
    for &(owner, name, _, flags) in RAW_ROWS {
        if NativeRowFlags(flags).contains(NativeRowFlags::DECLARED)
            && let Some(names) = tables[list_kind("declared", NativeRowFlags(flags))].get(owner)
            && !names.contains(name)
        {
            wrong.push(format!("{owner}.{name}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "DECLARED set on a name Rakudo's ^method_table lacks:\n{}",
        wrong.join("\n")
    );
}

#[test]
fn introspectable_bits_are_never_false_claims() {
    let tables = rakudo_tables();
    let mut wrong = Vec::new();
    for &(owner, name, _, flags) in RAW_ROWS {
        if introspection_reads_rows(owner)
            && NativeRowFlags(flags).contains(NativeRowFlags::INTROSPECTABLE)
            && let Some(names) = tables[list_kind("methods", NativeRowFlags(flags))].get(owner)
            && !names.contains(name)
        {
            wrong.push(format!("{owner}.{name}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "INTROSPECTABLE set on a name Rakudo's .^methods omits:\n{}",
        wrong.join("\n")
    );
}

/// A `REVISION_E` row is a name language revision 6.e adds to its owner:
/// Rakudo declares it under 6.e and not under 6.d. Always run: only a row that
/// sets the bit can fail it.
#[test]
fn revision_e_rows_are_6e_additions() {
    let tables = rakudo_tables();
    let mut wrong = Vec::new();
    for &(owner, name, _, flags) in RAW_ROWS {
        let flags = NativeRowFlags(flags);
        if flags.contains(NativeRowFlags::REVISION_E)
            && (!flags.contains(NativeRowFlags::DECLARED)
                || !tables["declared.e"]
                    .get(owner)
                    .is_some_and(|names| names.contains(name)))
        {
            wrong.push(format!("{owner}.{name}"));
        }
    }
    assert!(
        wrong.is_empty(),
        "REVISION_E set on a name 6.e does not add to its owner:\n{}",
        wrong.join("\n")
    );
}
