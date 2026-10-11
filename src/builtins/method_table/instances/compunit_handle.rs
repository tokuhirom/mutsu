//! `CompUnit::Handle.export-package` (ADR-11276, #11002).
//!
//! A `CompUnit::Handle` has no dispatch shape; the row is reached through its
//! owner (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the method-call
//! fallback, like the `RakuAST::Origin` rows. A handle of a loaded unit
//! carries the unit's name (`unit-name`); its export package is that unit's
//! `EXPORT` stash, a `Stash` of the `EXPORT::<tag>` packages.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "CompUnit::Handle",
    name: "export-package",
    arity: 0,
    handler: Handler::Interp(export_package),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// The owners the rows reachable from `target` are found under: a
/// `CompUnit::Handle` instance.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "CompUnit::Handle" => {
            Some(&["CompUnit::Handle"])
        }
        _ => None,
    }
}

// Cost: O(s), s = symbols in the unit's `EXPORT` package.
fn export_package(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let unit = attributes
        .as_map()
        .get("unit-name")
        .map(Value::to_string_value)?;
    Some(Ok(interp.package_stash_value(&format!("{unit}::EXPORT"))))
}
