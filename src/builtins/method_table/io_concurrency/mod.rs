//! The I/O and concurrency classes' rows (ADR-11276 §10, slice 3E).
//!
//! Owners: `IO::Path`, `IO::Handle`, `IO::Spec`, the socket classes,
//! `Proc::Async`, `Promise`, `Channel`, `Supply`, the schedulers, `Lock`. A
//! slice adds a family module here and lists it in [`FAMILIES`]; no other file
//! names it.

use super::MethodRow;

mod io_path_content;
mod io_path_ctx;
mod io_path_cwd;
mod io_path_fs;
mod io_path_lexical;
mod io_path_misc;
mod io_path_stat;
mod io_spec;

/// Every family of this group.
pub(super) static FAMILIES: &[&[MethodRow]] = &[
    io_path_lexical::ROWS,
    io_path_cwd::ROWS,
    io_path_stat::ROWS,
    io_path_content::ROWS,
    io_path_fs::ROWS,
    io_path_misc::ROWS,
    io_spec::ROWS,
];
