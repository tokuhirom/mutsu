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
//! `::(Owner).^methods` (`methods`). Two directions are checked:
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
//! Both checks are `#[ignore]`d and off the CI gate (#11405): coupling every
//! cascade change to a hand-edited shared row kept `main` red on 2026-10-03.
//! Run them on demand with
//! `cargo test --lib native_method_row_rakudo_oracle -- --ignored`.

use super::builtin_type_methods::{canonical_builtin_owner, native_method_arities};
use super::native_method_row::NativeRowFlags;
use super::native_method_row_table::RAW_ROWS;
use crate::value::Value;
use std::collections::{BTreeMap, BTreeSet, HashMap};

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

fn row_flags(owner: &str, name: &str) -> Option<(u8, NativeRowFlags)> {
    RAW_ROWS
        .iter()
        .find(|&&(o, n, _, _)| o == owner && n == name)
        .map(|&(_, _, arity, flags)| (arity, NativeRowFlags(flags)))
}

/// `.^methods` is served from the row catalog only for an owner that is its
/// own canonical owner (`builtin_type_methods::builtin_type_method_names`);
/// every other owner's `.^methods` comes from elsewhere, so its
/// `INTROSPECTABLE` bits are not read and not checked here.
fn introspection_reads_rows(owner: &str) -> bool {
    canonical_builtin_owner(owner) == owner
}

/// One Raku expression per owner whose value is a plain instance of it. An
/// owner without an entry is not probed (`Failure` would explode, `Junction`
/// would autothread the probe, `Mu`/`Any`/`Cool` have no instances of their
/// own).
const SAMPLES: &[(&str, &str)] = &[
    ("Str", "'abc'"),
    ("Int", "2"),
    ("Num", "1.5e0"),
    ("Rat", "1/2"),
    ("Complex", "1+2i"),
    ("Bool", "True"),
    ("List", "(1, 2, 3)"),
    ("Array", "[1, 2]"),
    ("Hash", "{a => 1}"),
    ("Map", "Map.new((a => 1))"),
    ("Pair", "a => 1"),
    ("Range", "1..3"),
    ("Seq", "(1, 2, 3).Seq"),
    ("Set", "set(1, 2)"),
    ("SetHash", "SetHash.new(1, 2)"),
    ("Bag", "bag(1, 2)"),
    ("BagHash", "BagHash.new(1, 2)"),
    ("Mix", "mix(1, 2)"),
    ("MixHash", "MixHash.new(1, 2)"),
    ("Instant", "Instant.from-posix(1)"),
    ("Duration", "Duration.new(1.5)"),
    ("Date", "Date.new(2024, 1, 2)"),
    ("DateTime", "DateTime.new(2024, 1, 2, 3, 4, 5)"),
    ("Version", "v1.2.3"),
    ("Capture", "\\(1, 2, a => 3)"),
    ("Match", "'foo' ~~ /f(o)(o)/"),
    ("IO::Path", "'tmp'.IO"),
    ("Code", "sub ($a) { $a }"),
    ("Uni", "Uni.new(97, 98)"),
];

/// Evaluates [`SAMPLES`] in one interpreter, keyed by owner.
fn sample_values() -> HashMap<&'static str, Value> {
    let mut interp = crate::runtime::Interpreter::new();
    SAMPLES
        .iter()
        .map(|&(owner, expr)| {
            interp
                .run(&format!("my $oracle-sample = {expr};"))
                .unwrap_or_else(|e| panic!("sample for {owner}: {e:?}"));
            let value = interp.env().get("oracle-sample").cloned().unwrap();
            (owner, value)
        })
        .collect()
}

#[test]
fn rakudo_snapshot_parses() {
    let tables = rakudo_tables();
    assert!(tables["declared"].contains_key("Str"));
    assert!(tables["declared"]["Rat"].contains("numerator"));
    assert!(tables["methods"]["Int"].contains("FatRat"));
}

/// `DECLARED` only: `INTROSPECTABLE` deliberately also marks names an owner
/// inherits (`List.map`, declared on `Any` in Rakudo), so it disagrees with
/// Rakudo's `.^methods` by design today -- tracked as #11272.
#[test]
#[ignore = "off the CI gate until the check is rebuilt: #11405"]
fn declared_bits_are_never_false_claims() {
    let tables = rakudo_tables();
    let mut wrong = Vec::new();
    for &(owner, name, _, flags) in RAW_ROWS {
        if NativeRowFlags(flags).contains(NativeRowFlags::DECLARED)
            && let Some(names) = tables["declared"].get(owner)
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
#[ignore = "off the CI gate until the check is rebuilt: #11405"]
fn recognized_rakudo_methods_are_never_denied() {
    let tables = rakudo_tables();
    let samples = sample_values();
    let mut missing = Vec::new();
    for (&owner, declared) in &tables["declared"] {
        let Some(sample) = samples.get(owner) else {
            continue;
        };
        let listed = tables["methods"].get(owner);
        let names: BTreeSet<&str> = declared
            .iter()
            .chain(listed.into_iter().flatten())
            .copied()
            .collect();
        for name in names {
            let observed = native_method_arities(sample, name);
            if observed == 0 {
                continue;
            }
            let mut want = NativeRowFlags(0);
            if declared.contains(name) {
                want = NativeRowFlags(want.0 | NativeRowFlags::DECLARED.0);
            }
            if introspection_reads_rows(owner) && listed.is_some_and(|l| l.contains(name)) {
                want = NativeRowFlags(want.0 | NativeRowFlags::INTROSPECTABLE.0);
            }
            let (arity, have) = row_flags(owner, name).unwrap_or((0, NativeRowFlags(0)));
            if have.0 & want.0 != want.0 {
                missing.push(format!(
                    "    (\"{owner}\", \"{name}\", {}, {}),",
                    arity | observed,
                    have.0 | want.0
                ));
            }
        }
    }
    assert!(
        missing.is_empty(),
        "{} row(s) deny a method Rakudo has and mutsu dispatches; \
         add or update in native_method_row_table.rs:\n{}",
        missing.len(),
        missing.join("\n")
    );
}
