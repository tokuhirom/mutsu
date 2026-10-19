//! Rows for methods Rakudo declares on a collection class whose answer needs
//! no machinery: the `of` of the quant hashes and `Pair.freeze`.

use super::{Handler, MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// `Setty.of`, `Baggy.of` and `Mixy.of` answer the shapes; the classes'
/// own declarations are found by owner only.
const OF: RowFlags = RowFlags::TYPE_OBJECT_OK.or(RowFlags::OWNER_ONLY);

pub(super) static ROWS: &[MethodRow] = &[
    // The value type a quant hash holds: a `Set` maps to `Bool`, a `Bag` to
    // `UInt`, a `Mix` to `Real` (the mutable forms too). A type object has one.
    row!("Set", "of", set_of, OF),
    row!("SetHash", "of", set_of, OF),
    row!("Bag", "of", bag_of, OF),
    row!("BagHash", "of", bag_of, OF),
    row!("Mix", "of", mix_of, OF),
    row!("MixHash", "of", mix_of, OF),
    // `Pair.freeze` answers the pair's value (`$!value`).
    row!("Pair", "freeze", freeze, RowFlags::NONE),
];

// Cost: O(1).
fn set_of(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::package(Symbol::intern("Bool")))
}

// Cost: O(1).
fn bag_of(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::package(Symbol::intern("UInt")))
}

// Cost: O(1).
fn mix_of(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::package(Symbol::intern("Real")))
}

// Cost: O(1).
fn freeze(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Pair(_, value) => Ok(value.clone()),
        ValueView::ValuePair(_, value) => Ok((*value).clone()),
        _ => Ok(target.clone()),
    }
}
