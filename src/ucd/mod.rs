//! Unicode Character Database lookups that do not depend on the runtime:
//! the general-category table, single-character numeric values and NFC.
//! The parser needs them for literals and identifiers, so they live below
//! it rather than in `builtins` (issue #10779); `builtins` re-exports them
//! under their old names.

pub(crate) mod gc;
mod gc_data;
pub(crate) mod normalize;
pub(crate) mod numeric;
pub(crate) mod numval_table;
// Test-only: the general-category generator/verifier and the table machinery
// it shares with `builtins::unicode_script_gen`. Declared with the `#[path]`
// form so `check-panic-surface` excludes them (their `expect`/`panic!` calls
// are test scaffolding).
#[cfg(test)]
#[path = "gc_gen.rs"]
mod gc_gen;
#[cfg(test)]
#[path = "table_gen.rs"]
pub(crate) mod table_gen;
