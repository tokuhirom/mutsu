//! `Collation`'s rows (ADR-11276 §9.131): `primary`, `secondary`, `tertiary`,
//! `quaternary`, `collation-level`, `set`, `gist`, and its `raku`.
//!
//! A `Collation` (`$*COLLATION`) is an instance keeping its four levels in its
//! shared attribute cell. `set` writes them there, so every alias of the
//! object sees the change and the row needs no receiver place (§9.126). The
//! class has no shape: the rows are reached through their owner from the
//! by-name dispatch. `raku` is mutsu's own: Rakudo's `Collation` holds one
//! `$!collation-level` attribute and its default `.raku` lists it, while mutsu
//! stores the four levels, so the row re-encodes the level.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::collation::CollationSettings;
use crate::builtins::method_table::Named;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:expr) => {
        row!($name, $handler, &[])
    };
    ($name:literal, $handler:expr, $named:expr) => {
        MethodRow {
            owner: "Collation",
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::OWNER_ONLY,
            named: $named,
        }
    };
}

/// The four levels, in `set`'s named arguments and the attributes.
const LEVELS: &[&str] = &["primary", "secondary", "tertiary", "quaternary"];

pub(super) static ROWS: &[MethodRow] = &[
    row!("primary", Handler::Narrow(primary)),
    row!("secondary", Handler::Narrow(secondary)),
    row!("tertiary", Handler::Narrow(tertiary)),
    row!("quaternary", Handler::Narrow(quaternary)),
    row!("collation-level", Handler::Narrow(collation_level)),
    row!("set", Handler::Named(set), LEVELS),
    row!("gist", Handler::Narrow(gist)),
    row!("raku", Handler::Narrow(raku)),
];

/// `Collation`'s rows for a `Collation` instance; `None` for anything else.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Collation" => Some(&["Collation"]),
        _ => None,
    }
}

/// One level's setting, 1 when unset.
// Cost: O(1).
fn level(target: &Value, name: &str) -> Answer {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(Ok(attributes
            .as_map()
            .get(name)
            .cloned()
            .unwrap_or(Value::int(1)))),
        _ => None,
    }
}

fn primary(target: &Value, _args: &[Value]) -> Answer {
    level(target, "primary")
}

fn secondary(target: &Value, _args: &[Value]) -> Answer {
    level(target, "secondary")
}

fn tertiary(target: &Value, _args: &[Value]) -> Answer {
    level(target, "tertiary")
}

fn quaternary(target: &Value, _args: &[Value]) -> Answer {
    level(target, "quaternary")
}

/// `Collation.collation-level`: the four levels as Rakudo's bit set.
// Cost: O(1).
fn collation_level(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::int(
        CollationSettings::from_value(target).collation_level(),
    )))
}

/// `Collation.set(:$primary, :$secondary, :$tertiary, :$quaternary)`: writes
/// the levels it is given (a `Bool` is 1 or 0) into the shared cell and
/// answers the collation.
// Cost: O(a), a = attributes of the instance.
fn set(target: &Value, _args: &[Value], named: Named<'_>) -> Answer {
    let ValueView::Instance {
        class_name,
        attributes,
        id,
    } = target.view()
    else {
        return None;
    };
    let mut updated = attributes.to_map();
    for name in LEVELS {
        if let Some(value) = named.get(name) {
            let level = match value.view() {
                ValueView::Bool(b) => i64::from(b),
                ValueView::Int(n) => n,
                _ => crate::runtime::utils::to_int(value),
            };
            updated.insert(name.to_string(), Value::int(level));
        }
    }
    Some(Ok(Value::write_back_sharing(
        &attributes,
        class_name,
        updated,
        id,
    )))
}

/// `Collation.gist`.
// Cost: O(1).
fn gist(target: &Value, _args: &[Value]) -> Answer {
    let settings = CollationSettings::from_value(target);
    Some(Ok(Value::str(format!(
        "collation-level => {}, Country => International, Language => None, primary => {}, secondary => {}, tertiary => {}, quaternary => {}",
        settings.collation_level(),
        settings.primary,
        settings.secondary,
        settings.tertiary,
        settings.quaternary
    ))))
}

/// `Collation.raku`: `Collation.new(collation-level => N)`.
// Cost: O(1).
fn raku(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(format!(
        "Collation.new(collation-level => {})",
        CollationSettings::from_value(target).collation_level()
    ))))
}
