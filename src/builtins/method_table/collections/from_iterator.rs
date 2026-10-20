//! `List.from-iterator` and `Array.from-iterator` (ADR-11276 §9.164): a type
//! object's constructor from an `Iterator` (`List:U: Iterator $iter`).
//!
//! A `Slip` inherits `List`'s and builds a `Slip`. The type objects reach the
//! rows through the class-method lookup (`RowFlags::CLASS_METHOD`); the handler is
//! `Interpreter::list_from_iterator`. `Seq` declares none (Rakudo builds a
//! `Seq` with `Seq.new($iterator)`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal) => {
        MethodRow {
            owner: $owner,
            name: "from-iterator",
            arity: 1,
            handler: Handler::Interp(from_iterator),
            flags: RowFlags::OWNER_ONLY
                .or(RowFlags::TYPE_OBJECT_OK)
                .or(RowFlags::CLASS_METHOD)
                .or(RowFlags::ANY_ARGS)
                .or(RowFlags::OBJECT_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("List"), row!("Array")];

// Cost: as `Interpreter::list_from_iterator`.
fn from_iterator(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Package(name) = target.view() else {
        return None;
    };
    Some(interp.list_from_iterator(name.as_str(), &args[0]))
}
