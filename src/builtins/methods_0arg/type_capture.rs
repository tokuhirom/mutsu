//! `T.Capture` of a type object (#18778), from `builtin_type_capture.tsv`,
//! which `scripts/builtin-type-capture.raku` reads off rakudo's core classes.
//!
//! Rakudo's `Mu.Capture` reads each public attribute of `self`: on a type
//! object of a class with no attributes that is `\()`, and on one with
//! attributes the read dies ("Cannot look up attributes in a T type object").
//! A class whose own `Capture` is `:D` (`List`, `Map`, `Range`, ...) refuses a
//! type object as its invocant. A user class decides the same way over its MRO
//! (`Interpreter::type_object_capture`).

use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueMap};
use rustc_hash::FxHashMap;
use std::sync::OnceLock;

/// What a core class's type object does for `.Capture`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TypeCapture {
    /// `Mu.Capture` over a class with no attributes: `\()`.
    Empty,
    /// `Mu.Capture` over a class with attributes: the attribute read dies.
    Attributes,
    /// The class's own `Capture` is `:D`: the invocant is refused.
    Instance,
}

static TSV: &str = include_str!("builtin_type_capture.tsv");

// Cost: O(r), r = rows of the table, once per process.
fn table() -> &'static FxHashMap<&'static str, TypeCapture> {
    static TABLE: OnceLock<FxHashMap<&'static str, TypeCapture>> = OnceLock::new();
    TABLE.get_or_init(|| {
        TSV.lines()
            .filter_map(|line| {
                let (name, kind) = line.split_once('\t')?;
                let kind = match kind {
                    "empty" => TypeCapture::Empty,
                    "attributes" => TypeCapture::Attributes,
                    "instance" => TypeCapture::Instance,
                    _ => return None,
                };
                Some((name, kind))
            })
            .collect()
    })
}

/// What the core class `name`'s type object does for `.Capture`; `None` for a
/// class the table does not list.
// Cost: O(1), one hash probe.
pub(crate) fn builtin_kind(name: &str) -> Option<TypeCapture> {
    table().get(name).copied()
}

/// The answer of `.Capture` on the type object `got` whose MRO decided `kind`
/// at the class `decided_by` (`got` itself, or the ancestor that declares the
/// refusing `Capture`).
// Cost: O(1) (O(m) to build an error message, m = its chars).
pub(crate) fn answer(
    kind: TypeCapture,
    got: &str,
    decided_by: &str,
) -> Result<Value, RuntimeError> {
    match kind {
        TypeCapture::Empty => Ok(Value::capture(Vec::new(), ValueMap::default())),
        TypeCapture::Attributes => Err(RuntimeError::new(format!(
            "Cannot look up attributes in a {got} type object. Did you forget a '.new'?"
        ))),
        TypeCapture::Instance => Err(RuntimeError::parameter_invalid_concreteness(
            decided_by, got, "Capture", "self", true, true,
        )),
    }
}

/// `.Capture` of the core type object `name`, when the table lists it.
// Cost: O(1).
pub(crate) fn builtin_type_object_capture(name: Symbol) -> Option<Result<Value, RuntimeError>> {
    let name = name.resolve();
    builtin_kind(&name).map(|kind| answer(kind, &name, &name))
}
