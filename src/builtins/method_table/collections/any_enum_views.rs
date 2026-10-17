//! `Any`'s key/value views of an enumeration (ADR-11276 §9.145): the
//! `Any(Enumeration)`, `Any(Bool)` and `Any:U` candidates Rakudo declares
//! for `pairs`, `keys`, `values`, `kv`, `antipairs` and `invert`.
//!
//! An enum's type object and each of its values answer the view of the enum's
//! `enums` map (`Color.pairs`, `red.keys`); a defined value's `kv` is
//! `Enumeration.kv`, its own key and value, which the enumeration roles'
//! rows answer first. Any other type object answers the empty `List`
//! (`Int.pairs`). The enum's values come from the interpreter's registry, so
//! these rows are `Handler::Interp`; the `Bool` values' views stay in the pure
//! `Any` rows and share [`view`] with them.
//!
//! The rows are `OWNER_ONLY`: [`invoke`] reaches them for a type object from
//! the by-name dispatch and for an enum value from the enum dispatch
//! (`dispatch_enum_method`), after the enumeration roles' rows.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{EnumValue, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const VIEW: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

macro_rules! view_row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Any",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: VIEW,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    view_row!("pairs", pairs),
    view_row!("keys", keys),
    view_row!("values", values),
    view_row!("kv", kv),
    view_row!("antipairs", antipairs),
    view_row!("invert", invert),
];

/// Answer `method` from these rows, or `None` when no row of that name and
/// arity binds the call (the caller's dispatch goes on).
// Cost: O(r) to find the row, r = 6 rows, plus the row's own.
pub(crate) fn invoke(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Answer {
    let row = ROWS
        .iter()
        .find(|row| row.name == method && usize::from(row.arity) == args.len())?;
    let Handler::Interp(handler) = row.handler else {
        return None;
    };
    handler(interp, target, args, Named::NONE)
}

/// One of the six views.
#[derive(Clone, Copy)]
pub(crate) enum View {
    Pairs,
    Keys,
    Values,
    Kv,
    Antipairs,
    Invert,
}

/// The `view` of an enumeration whose `enums` map holds `variants`, in
/// declaration order: a `Seq`, as `self.enums.METHOD` is in Rakudo. An
/// `invert` lists a positional value's elements one pair each.
// Cost: O(v), v = values of the enumeration (their elements for `invert`).
pub(crate) fn view(view: View, variants: &[(String, EnumValue)]) -> Value {
    let key = |k: &String| Value::str(k.clone());
    let items: Vec<Value> = match view {
        View::Pairs => variants
            .iter()
            .map(|(k, v)| Value::value_pair(key(k), v.to_value()))
            .collect(),
        View::Keys => variants.iter().map(|(k, _)| key(k)).collect(),
        View::Values => variants.iter().map(|(_, v)| v.to_value()).collect(),
        View::Kv => variants
            .iter()
            .flat_map(|(k, v)| [key(k), v.to_value()])
            .collect(),
        View::Antipairs => variants
            .iter()
            .map(|(k, v)| Value::value_pair(v.to_value(), key(k)))
            .collect(),
        View::Invert => variants
            .iter()
            .flat_map(|(k, v)| {
                let value = v.to_value();
                let elements = match value.view() {
                    ValueView::Array(items, _) => items.to_vec(),
                    _ => vec![value.clone()],
                };
                elements
                    .into_iter()
                    .map(move |element| Value::value_pair(element, key(k)))
            })
            .collect(),
    };
    Value::seq(items)
}

/// `Bool.enums`, in declaration order.
// Cost: O(1).
pub(crate) fn bool_variants() -> [(String, EnumValue); 2] {
    [
        ("False".to_string(), EnumValue::Int(0)),
        ("True".to_string(), EnumValue::Int(1)),
    ]
}

/// The enumeration's view for an enum's type object or value, `()` for any
/// other type object (`Any:U`), and `None` for any other receiver.
// Cost: O(v), v = values of the enumeration.
fn answer(interp: &Interpreter, target: &Value, kind: View) -> Answer {
    if let Some(variants) = interp.enumeration_variants(target) {
        return Some(Ok(view(kind, &variants)));
    }
    matches!(target.view(), ValueView::Package(_)).then(|| Ok(Value::array(Vec::new())))
}

// Cost: O(v), v = values of the enumeration.
fn pairs(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    answer(i, t, View::Pairs)
}

// Cost: O(v), v = values of the enumeration.
fn keys(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    answer(i, t, View::Keys)
}

// Cost: O(v), v = values of the enumeration.
fn values(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    answer(i, t, View::Values)
}

/// `kv` of a defined enum value is `Enumeration.kv` (its own key and value);
/// only the type object lists the whole enumeration.
// Cost: O(v), v = values of the enumeration.
fn kv(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if matches!(t.view(), ValueView::Enum { .. }) {
        return None;
    }
    answer(i, t, View::Kv)
}

// Cost: O(v), v = values of the enumeration.
fn antipairs(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    answer(i, t, View::Antipairs)
}

// Cost: O(e), e = the enumeration's values and their elements.
fn invert(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    answer(i, t, View::Invert)
}
