//! The I/O and concurrency classes' rows (ADR-11276 §10, slice 3E).
//!
//! Owners: `IO::Path`, `IO::Handle`, `IO::Spec`, the socket classes,
//! `Proc::Async`, `Promise`, `Channel`, `Supply`, the schedulers, `Lock`. A
//! slice adds a family module here and lists it in [`FAMILIES`]; no other file
//! names it.

use super::MethodRow;

pub(crate) mod cathandle;
pub(crate) mod class_methods;
mod concurrency_state;
pub(crate) mod encoding;
mod io_handle;
mod io_path_content;
mod io_path_ctx;
mod io_path_cwd;
mod io_path_fs;
pub(crate) mod io_path_lexical;
mod io_path_misc;
pub(crate) mod io_path_parts;
mod io_path_stat;
mod io_spec;
pub(crate) mod io_special;
mod lock_rows;
pub(crate) mod proc_async;
pub(crate) mod promise_channel;
mod promise_class;
mod socket_rows;
pub(crate) mod supplier_scheduler;
pub(crate) mod systemic;
mod tap_proc_rows;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    io_path_lexical::ROWS,
    io_path_cwd::ROWS,
    io_path_stat::ROWS,
    io_path_content::ROWS,
    io_path_fs::ROWS,
    io_path_misc::ROWS,
    io_path_parts::ROWS,
    io_spec::ROWS,
    io_handle::ROWS,
    io_special::ROWS,
    encoding::ROWS,
    systemic::ROWS,
    lock_rows::ROWS,
    tap_proc_rows::ROWS,
    socket_rows::ROWS,
    concurrency_state::ROWS,
    promise_channel::ROWS,
    promise_class::ROWS,
    proc_async::ROWS,
    cathandle::ROWS,
    supplier_scheduler::ROWS,
    class_methods::ROWS,
];

/// The methods the handle class `owner` declares: the native methods
/// `runtime_init` registers for it, derived so they cannot drift from the rows.
// Cost: O(r), r = rows of the two families.
pub(crate) fn handle_names(owner: &str) -> impl Iterator<Item = &'static str> + '_ {
    lock_rows::ROWS
        .iter()
        .chain(tap_proc_rows::ROWS)
        .chain(socket_rows::ROWS)
        .filter(move |row| row.owner == owner)
        .map(|row| row.name)
}
