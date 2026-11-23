//! The attributes rakudo's built-in exception classes declare (`X::NYI`'s
//! `$.feature`, `$.did-you-mean`, `$.workaround`, ...), from
//! `builtin_exception_attrs.tsv`, which `scripts/builtin-exception-attrs.raku`
//! reads off rakudo's own classes (`.^attributes(:local)`, the attributes
//! `.new` builds, and their literal initializers).
//!
//! `runtime_init` marks each built-in exception class, and its
//! `ClassDef::attributes` declares them on first access ([`declare`],
//! `class_attributes`), so `.^attributes`, the accessors, construction and the
//! default `.raku` see them (#18654) while a program that never touches an
//! exception class does not pay for them (#18956). An unset slot holds its type object, which the built-in
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

/// Whether built-in class `class` may have table rows: the classes the table
/// lists are all `X::` and `CX::` exceptions. Answers without parsing the
/// table.
// Cost: O(1).
pub(crate) fn may_declare(class: &str) -> bool {
    class.starts_with("X::") || class.starts_with("CX::")
}

/// Declares built-in exception class `class`'s attributes after `attrs`, the
/// ones it already has (skipping a name already declared), and returns the
/// private ones built from their named argument. The few private `is built`
/// ones (`X::TypeCheck`'s `$!got`/`$!expected`, `X::Coerce::Role`'s
/// `$!exception`, ...) each have an explicit reader method in rakudo, so they
/// are declared with their accessor. An attribute a built-in method row
/// already reads (`X::AdHoc.payload`, which falls back to the message an
/// internal error carries) is built from its named argument but gets no
/// generated accessor, so the row answers.
// Cost: O(r * a), r = the class's table rows, a = attributes already declared
// (plus the one-time table parse).
pub(crate) fn declare(
    class: &str,
    attrs: &mut Vec<super::ClassAttributeDef>,
) -> Vec<&'static str> {
    let mut built = Vec::new();
    let Some(rows) = table().get(class) else {
        return built;
    };
    for row in rows {
        if attrs.iter().any(|a| a.name == row.name) {
            continue;
        }
        let build = row.build_default();
        let row_reads_it = crate::builtins::method_table::owner_has_row(class, row.name);
        if row_reads_it {
            built.push(row.name);
        }
        attrs.push(super::ClassAttributeDef {
            name: row.name.to_string(),
            is_public: !row_reads_it,
            default: build
                .clone()
                .or_else(|| row.unset_seed())
                .map(crate::opcode::DeclTraitArg::Literal),
            captured_env: None,
            captured_unit: None,
            declaring_package: None,
            is_rw: false,
            is_required: None,
            sigil: row.sigil(),
            type_constraint: None,
            where_constraint: None,
            declared_shape: None,
            source_line: None,
            source_file: None,
            default_is_seed: build.is_none(),
            default_is_trait: false,
        });
    }
    built
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
