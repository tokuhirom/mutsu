//! The list terminals on the owners Rakudo declares them on (ADR-11276 slice 3C
//! remainder): the handlers are the ones `Any`, `List` and `Seq` already use,
//! registered once more on the declaring owner so the table, `.^can` and the
//! resolver see them where Rakudo does. Every handler declines what it does not
//! cover, so a receiver it declines takes the cascade as before.

use super::{Handler, MethodRow, RowFlags, list, list_aggregate, list_transform};

const fn narrow(
    owner: &'static str,
    name: &'static str,
    arity: u8,
    handler: crate::builtins::method_table::NarrowFn,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity,
        handler: Handler::Narrow(handler),
        flags: RowFlags::NONE,
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    narrow("List", "head", 1, list::head),
    narrow("Map", "head", 1, list::head),
    narrow("List", "tail", 1, list::tail),
    narrow("Array", "tail", 1, list::tail),
    narrow("List", "sum", 0, list_aggregate::sum),
    list_transform::flat_row("Map"),
    list_transform::flat_row("Range"),
];
