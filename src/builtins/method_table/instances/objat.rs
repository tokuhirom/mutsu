//! `ObjAt`'s rows (also `ValueObjAt`'s): `Str`, `Stringy`, `gist` and `raku`.
//!
//! An `ObjAt` (what `.WHICH` returns) carries its identity text as an
//! attribute (`AttrMap::objat_which`), so every handler is a pure read of it.
//! Without these rows `.Str` on one fell through the whole general method
//! cascade -- MRO walks, builtin-shadowing checks, the native fallbacks --
//! at ~120k instructions a call, against Rakudo's ~0.3 us; Monad builds
//! every type-cache key as `M.WHICH ~ '|' ~ T.WHICH` (#17331).
//!
//! Both classes are opaque dispatch shapes (`dispatch_shape/opaque.rs`)
//! whose rows are `ObjAt`'s. A role mixed in (`ValueObjAt+{R}`) is another
//! class name and keeps the general dispatch, which answers the same text.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("ObjAt", "Str", str_),
    row!("ObjAt", "Stringy", str_),
    row!("ObjAt", "gist", str_),
    row!("ObjAt", "raku", raku),
];

/// The class name and identity text of an exact `ObjAt` / `ValueObjAt`
/// instance; `None` for any other receiver (a type object, a mixin).
// Cost: O(n), n = chars of the identity text.
fn objat(target: &Value) -> Option<(&'static str, Option<String>)> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            let class = if class_name == "ValueObjAt" {
                "ValueObjAt"
            } else if class_name == "ObjAt" {
                "ObjAt"
            } else {
                return None;
            };
            Some((class, attributes.as_map().objat_which()))
        }
        _ => None,
    }
}

// Cost: O(n), n = chars of the identity text.
fn str_(target: &Value, _args: &[Value]) -> Answer {
    objat_str(target).map(Ok)
}

/// `.Str` of an exact `ObjAt` / `ValueObjAt` instance: its identity text.
/// `None` for any other receiver. The one implementation behind the `Str`
/// row above and the string-context coercion of `~` / `eq`
/// (`coerce_stringy_operand`), which used to reach it only through a full
/// method dispatch (#17331).
// Cost: O(n), n = chars of the identity text.
pub(crate) fn objat_str(target: &Value) -> Option<Value> {
    let (class, which) = objat(target)?;
    Some(Value::str(which.unwrap_or_else(|| format!("{class}()"))))
}

// Cost: O(n), n = chars of the identity text.
fn raku(target: &Value, _args: &[Value]) -> Answer {
    let (class, which) = objat(target)?;
    Some(Ok(Value::str(crate::value::raku_repr::objat_raku(
        class, which,
    ))))
}
