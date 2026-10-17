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
pub(crate) mod proc_async;
pub(crate) mod promise_channel;
mod promise_class;
pub(crate) mod supplier_scheduler;
pub(crate) mod systemic;

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
    concurrency_state::ROWS,
    promise_channel::ROWS,
    promise_class::ROWS,
    proc_async::ROWS,
    cathandle::ROWS,
    supplier_scheduler::ROWS,
    class_methods::ROWS,
];
