//! The attributes rakudo's built-in exception classes declare (`X::NYI`'s
//! `$.feature`, `$.did-you-mean`, `$.workaround`, ...), from
//! `builtin_exception_attrs.tsv`, which `scripts/builtin-exception-attrs.raku`
//! reads off rakudo's own classes (`.^attributes(:local)`, the attributes
//! `.new` builds, and their literal initializers).
//!
//! `runtime_init` declares them as the classes' `ClassDef::attributes`, so
//! `.^attributes`, the accessors, construction and the default `.raku` see
//! them (#18654). An unset slot holds its type object, which the built-in
//! message formatters read as absent (`value::exception_message::defined_attr`).

use crate::value::Value;
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// One declared attribute: its bare name, and the type object an unset scalar
/// holds (`Any`, `Mu`, `Bool`, `Nil`; empty for an `@`/`%` attribute).
pub(crate) struct Row {
    pub(crate) name: &'static str,
    sigil: char,
    default: &'static str,
    build: &'static str,
}

static TSV: &str = include_str!("builtin_exception_attrs.tsv");

// Cost: O(r), r = rows of the table, once per process.
fn table() -> &'static FxHashMap<&'static str, Vec<Row>> {
    static TABLE: OnceLock<FxHashMap<&'static str, Vec<Row>>> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table: FxHashMap<&'static str, Vec<Row>> = FxHashMap::default();
        for line in TSV.lines() {
            let mut cols = line.split('\t');
            let (Some(class), Some(sigil), Some(name), Some(_public)) =
                (cols.next(), cols.next(), cols.next(), cols.next())
            else {
                continue;
            };
            table.entry(class).or_default().push(Row {
                name,
                sigil: sigil.chars().next().unwrap_or('$'),
                default: cols.next().unwrap_or(""),
                build: cols.next().unwrap_or(""),
            });
        }
        table
    })
}

/// Every class the table lists, with the attributes it declares.
// Cost: O(c), c = classes in the table.
pub(crate) fn all() -> impl Iterator<Item = (&'static str, &'static [Row])> {
    table().iter().map(|(class, rows)| (*class, rows.as_slice()))
}

impl Row {
    /// The `@`, `%` or `$` sigil of the declaration.
    // Cost: O(1).
    pub(crate) fn sigil(&self) -> char {
        self.sigil
    }

    /// What an unset scalar slot holds when it is not `Any` (`has Bool
    /// $.private` holds `Bool`, `has $.payload is default(Nil)` holds `Nil`,
    /// `has str $.name` holds `""`); `None` for `Any` and for `@`/`%`.
    ///
    /// TODO: this seeds the slot but does not declare the type constraint, so
    /// a mistyped argument is not rejected as rakudo's would be. Declaring it
    /// needs every internal throw site checked against the declared types.
    // Cost: O(1).
    pub(crate) fn unset_seed(&self) -> Option<Value> {
        match self.default {
            "" | "Any" => None,
            "Nil" => Some(Value::NIL),
            "int" => Some(Value::int(0)),
            "str" => Some(Value::str_from("")),
            ty => Some(Value::package(crate::symbol::Symbol::intern(ty))),
        }
    }

    /// The literal initializer construction applies when no named argument
    /// sets the attribute (`has $.what = 'symbol'`, `has Bool $.is-compile-time
    /// = False`); `None` when the declaration has none or computes it.
    // Cost: O(s), s = the initializer's length.
    pub(crate) fn build_default(&self) -> Option<Value> {
        if let Some(text) = self.build.strip_prefix("Str:") {
            return Some(Value::str_from(text));
        }
        match self.build {
            "Bool:True" => Some(Value::truth(true)),
            "Bool:False" => Some(Value::truth(false)),
            _ => None,
        }
    }
}
