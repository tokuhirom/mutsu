//! The rows of the roles Rakudo composes into every enum type (ADR-11276
//! §9.125): `Enumeration`'s `key`, `value`, `kv`, `pair`, `gist`, `raku`,
//! `pred`, `succ`, `ACCEPTS`, `Int`, `Numeric` and `Real`, and the `Str` of
//! `NumericEnumeration` (an `Int`-valued enum: the key) and
//! `StringyEnumeration` (a `Str`-valued one: the value).
//!
//! Rakudo composes the roles into each enum type, so `E.^method_table` lists
//! them; mutsu's enum member is one value kind (`ValueView::Enum`) for every
//! enum, so the rows are the roles' own, found by their owner
//! (`RowFlags::OWNER_ONLY`) under the owners of the opaque shape of a member
//! by its payload kind (`DispatchShape::EnumNumeric`, `EnumStringy`,
//! `EnumGeneric`; the more specific role first), after the methods of a role
//! the enum itself composes (`dispatch_enum_role_method`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{EnumValue, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Enumeration", "key", 0, Handler::Narrow(key)),
    row!("Enumeration", "value", 0, Handler::Narrow(value)),
    row!("Enumeration", "kv", 0, Handler::Narrow(kv)),
    row!("Enumeration", "pair", 0, Handler::Narrow(pair)),
    row!("Enumeration", "gist", 0, Handler::Narrow(key)),
    row!("Enumeration", "raku", 0, Handler::Narrow(raku)),
    row!("Enumeration", "pred", 0, Handler::Interp(pred)),
    row!("Enumeration", "succ", 0, Handler::Interp(succ)),
    row!("Enumeration", "ACCEPTS", 1, Handler::Interp(accepts)),
    row!("Enumeration", "Int", 0, Handler::Interp(int)),
    row!("Enumeration", "Numeric", 0, Handler::Interp(numeric)),
    row!("Enumeration", "Real", 0, Handler::Interp(real)),
    row!("NumericEnumeration", "Str", 0, Handler::Narrow(key)),
    row!("StringyEnumeration", "Str", 0, Handler::Narrow(stringy_str)),
];

/// The member's key, its value and the index of the member in its enum.
// Cost: O(1).
fn parts(target: &Value) -> Option<(Symbol, Symbol, &EnumValue, usize)> {
    match target.view() {
        ValueView::Enum {
            enum_type,
            key,
            value,
            index,
        } => Some((enum_type, key, value, index)),
        _ => None,
    }
}

/// `Enumeration.key` (and `gist`, and an `Int`-valued member's `Str`).
// Cost: O(k), k = chars of the key.
fn key(target: &Value, _args: &[Value]) -> Answer {
    let (_, key, _, _) = parts(target)?;
    Some(Ok(Value::str(key.resolve())))
}

/// `Enumeration.value`.
// Cost: O(1).
fn value(target: &Value, _args: &[Value]) -> Answer {
    let (_, _, value, _) = parts(target)?;
    Some(Ok(value.to_value()))
}

/// `Enumeration.kv`: the key and the value.
// Cost: O(k), k = chars of the key.
fn kv(target: &Value, _args: &[Value]) -> Answer {
    let (_, key, value, _) = parts(target)?;
    Some(Ok(Value::array(vec![
        Value::str(key.resolve()),
        value.to_value(),
    ])))
}

/// `Enumeration.pair`: `key => value`. A data-minted pair defaults positional
/// (ADR-0021 I2).
// Cost: O(k), k = chars of the key.
fn pair(target: &Value, _args: &[Value]) -> Answer {
    let (_, key, value, _) = parts(target)?;
    Some(Ok(Value::value_pair(
        Value::str(key.resolve()),
        value.to_value(),
    )))
}

/// `Enumeration.raku`: `Type::key`.
// Cost: O(k), k = chars of the name.
fn raku(target: &Value, _args: &[Value]) -> Answer {
    let (enum_type, key, _, _) = parts(target)?;
    Some(Ok(Value::str(
        crate::builtins::methods_0arg::raku_repr::enum_raku_repr(
            &enum_type.resolve(),
            &key.resolve(),
        ),
    )))
}

/// `StringyEnumeration.Str`: the member's string value. A member of another
/// kind declines.
// Cost: O(n), n = chars of the value.
fn stringy_str(target: &Value, _args: &[Value]) -> Answer {
    let (_, _, value, _) = parts(target)?;
    match value {
        EnumValue::Str(s) => Some(Ok(Value::str(s.clone()))),
        _ => None,
    }
}

/// The member `offset` places from `target` in its enum, or `target` itself
/// past either end (`.pred` of the first member, `.succ` of the last).
// Cost: O(1), one registry lookup.
fn neighbour(interp: &Interpreter, target: &Value, offset: isize) -> Answer {
    let (enum_type, _, _, index) = parts(target)?;
    let registry = interp.registry();
    let neighbour = index
        .checked_add_signed(offset)
        .and_then(|i| {
            let variants = registry.enum_types.get(&enum_type.resolve())?;
            let (key, value) = variants.get(i)?;
            Some(Value::enum_parts(
                enum_type,
                Symbol::intern(key),
                value.clone(),
                i,
            ))
        })
        .unwrap_or_else(|| target.clone());
    Some(Ok(neighbour))
}

/// `Enumeration.pred`.
// Cost: O(1).
fn pred(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    neighbour(i, t, -1)
}

/// `Enumeration.succ`.
// Cost: O(1).
fn succ(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    neighbour(i, t, 1)
}

/// `Enumeration.ACCEPTS($topic)`: an enum member is its base type's value, so
/// it answers that type's `ACCEPTS` (`Less.ACCEPTS(-1)` is `Int`'s, `True`): the
/// smartmatch of the topic against the member, which compares by value (#13333).
// Cost: the smartmatch's, O(1) for a numeric member.
fn accepts(i: &mut Interpreter, t: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    Some(Ok(Value::truth(
        crate::builtins::method_table::accepts_by_smartmatch(i, &args[0], t),
    )))
}

/// A numeric coercion of a member acts on its underlying value: `cool.Int` for
/// `enum Numbers (cool => '42')` is `'42'.Int`, and `sqrt-n-one.Real` for an
/// `'i'` value raises the string-numeric error, as Rakudo does.
// Cost: the value's coercion.
fn coerce(i: &mut Interpreter, target: &Value, method: &str) -> Answer {
    let (_, _, value, _) = parts(target)?;
    Some(i.call_method_with_values(value.to_value(), method, Vec::new()))
}

/// `Enumeration.Int`.
// Cost: the value's coercion.
fn int(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    coerce(i, t, "Int")
}

/// `Enumeration.Numeric`.
// Cost: the value's coercion.
fn numeric(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    coerce(i, t, "Numeric")
}

/// `Enumeration.Real`.
// Cost: the value's coercion.
fn real(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    coerce(i, t, "Real")
}
