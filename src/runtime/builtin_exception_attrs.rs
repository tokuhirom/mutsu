//! The attributes rakudo's built-in exception classes declare (`X::NYI`'s
//! `$.feature`, `$.did-you-mean`, `$.workaround`, ...), from
//! `builtin_exception_attrs.tsv`, which `scripts/builtin-exception-attrs.raku`
//! reads off rakudo's own classes (`.^attributes(:local)`, the attributes
//! `.new` builds).
//!
//! They render a built-in exception's default `.raku`/`.gist`
//! (`X::NYI.new(feature => "x", did-you-mean => Any, workaround => Any)`), as
//! Rakudo's `Mu.raku` lists every buildable attribute, set or not.
//!
//! TODO: declare these as the classes' `ClassDef::attributes`, so `.^attributes`
//! and construction see them too. That needs the built-in message formatters
//! (`value::exception_message`, the runtime's `message` builders) to read an
//! unset attribute's type object as absent, as Rakudo's do with `.defined`:
//! today they test the attribute map for presence, and construction would
//! seed every declared slot (#18654).

use crate::value::{AttrMap, Value};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// One declared attribute: its bare name, and the type object an unset scalar
/// holds (`Any`, `Mu`, `Bool`, `Nil`; empty for an `@`/`%` attribute).
pub(crate) struct Row {
    pub(crate) name: &'static str,
    default: &'static str,
}

static TSV: &str = include_str!("builtin_exception_attrs.tsv");

// Cost: O(r), r = rows of the table, once per process.
fn table() -> &'static FxHashMap<&'static str, Vec<Row>> {
    static TABLE: OnceLock<FxHashMap<&'static str, Vec<Row>>> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table: FxHashMap<&'static str, Vec<Row>> = FxHashMap::default();
        for line in TSV.lines() {
            let mut cols = line.split('\t');
            let (Some(class), Some(_sigil), Some(name), Some(_public)) =
                (cols.next(), cols.next(), cols.next(), cols.next())
            else {
                continue;
            };
            table.entry(class).or_default().push(Row {
                name,
                default: cols.next().unwrap_or(""),
            });
        }
        table
    })
}

/// The attributes `class` itself declares, in declaration order; `None` when
/// the table does not list it.
// Cost: O(1), one hash probe.
pub(crate) fn declared(class: &str) -> Option<&'static [Row]> {
    table().get(class).map(Vec::as_slice)
}

impl Row {
    /// The attribute's value on an instance whose attribute map is `attrs`:
    /// the stored one, else the type object an unset slot holds.
    // Cost: O(1).
    pub(crate) fn value(&self, attrs: &AttrMap) -> Value {
        if let Some(value) = attrs.get(self.name) {
            return value.clone();
        }
        match self.default {
            "" | "Nil" => Value::NIL,
            ty => Value::package(crate::symbol::Symbol::intern(ty)),
        }
    }
}
