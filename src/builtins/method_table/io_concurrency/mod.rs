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
mod io_path_parts;
mod io_path_stat;
mod io_spec;
mod io_special;
mod lock_rows;
pub(crate) mod proc_async;
pub(crate) mod promise_channel;
mod promise_class;
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
    concurrency_state::ROWS,
    promise_channel::ROWS,
    promise_class::ROWS,
    proc_async::ROWS,
    cathandle::ROWS,
    supplier_scheduler::ROWS,
    class_methods::ROWS,
];

/// The owners whose rows answer an instance of one of the handle classes,
/// `Lock`, `Lock::ConditionVariable`, `Cancellation`, `Tap` and `Proc` and
/// their kin (ADR-11276 §9.157).
// Cost: O(1).
pub(crate) fn handle_owners_of(class: &str) -> Option<&'static [&'static str]> {
    lock_rows::owners_of(class).or_else(|| tap_proc_rows::owners_of(class))
}

/// The methods the handle class `owner` declares: the native methods
/// `runtime_init` registers for it, derived so they cannot drift from the rows.
// Cost: O(r), r = rows of the two families.
pub(crate) fn handle_names(owner: &str) -> impl Iterator<Item = &'static str> + '_ {
    lock_rows::ROWS
        .iter()
        .chain(tap_proc_rows::ROWS)
        .filter(move |row| row.owner == owner)
        .map(|row| row.name)
}

/// [`handle_owners_of`] for an instance of one of the handle classes themselves
/// (a user subclass reaches them through the native dispatcher instead).
// Cost: O(1).
pub(crate) fn handle_owners_of_instance(
    target: &crate::value::Value,
) -> Option<&'static [&'static str]> {
    match target.view() {
        crate::value::ValueView::Instance { class_name, .. } => {
            handle_owners_of(&class_name.resolve())
        }
        _ => None,
    }
}
