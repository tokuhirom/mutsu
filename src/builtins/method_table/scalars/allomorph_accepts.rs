//! `Allomorph.ACCEPTS` against an object argument (ADR-11276 §9.191).
//!
//! Rakudo's `Allomorph.ACCEPTS(Allomorph:D: Any:D \a)` matches a `Numeric`
//! argument with the number, a `Str` with the string, and anything else with
//! both (`self.Str.ACCEPTS(a) && self.Numeric.ACCEPTS(a)`), so an object
//! whose `.Str` and `.Numeric` are user methods is matched through them. The
//! pure arm (`methods_narg::allomorph::allomorph_accepts`) declines such an
//! argument; this late interpreter twin, reached through the late MRO walk of
//! an allomorph receiver, answers it by calling the parts' own `ACCEPTS`.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Allomorph",
    name: "ACCEPTS",
    arity: 1,
    handler: Handler::Interp(accepts),
    flags: RowFlags::OWNER_ONLY.or(RowFlags::OBJECT_ARGS),
    named: &[],
}];

/// The string and the number of a plain allomorph receiver.
// Cost: O(1).
fn parts(target: &Value) -> Option<(Value, Value)> {
    let ValueView::Mixin(number, mixins) = target.view() else {
        return None;
    };
    let string = mixins.get("Str")?;
    matches!(string.view(), ValueView::Str(_)).then(|| (string.clone(), number.as_ref().clone()))
}

/// `Allomorph.ACCEPTS($object)`: `None` for any other argument, which the
/// pure arm answers.
// Cost: the argument's `.Str` and/or `.Numeric` and the parts' `ACCEPTS`.
fn accepts(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let [arg] = args else {
        return None;
    };
    if !matches!(arg.view(), ValueView::Instance { .. }) {
        return None;
    }
    let (string, number) = parts(target)?;
    let accepts = |interp: &mut Interpreter, part: Value| {
        interp
            .call_method_with_values(part, "ACCEPTS", vec![arg.clone()])
            .map(|answer| answer.truthy())
    };
    Some((|| {
        let matched = if interp.type_matches_value("Numeric", arg) {
            accepts(interp, number)?
        } else if interp.type_matches_value("Str", arg) {
            accepts(interp, string)?
        } else {
            accepts(interp, string)? && accepts(interp, number)?
        };
        Ok(Value::truth(matched))
    })())
}
