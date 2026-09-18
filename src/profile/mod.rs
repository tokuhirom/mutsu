//! The Raku-level profiler (ADR-0106).
//!
//! Two halves, deliberately different in kind (ADR-0106 D1):
//!
//! - [`counts`] — **exact** per-line hits, per-routine entries and
//!   per-callsite calls, taken at chokepoints the VM already runs through.
//!   Deterministic, load-independent, and the only thing a test may assert
//!   (D5).
//! - [`sampler`] — **sampled** time. A timer thread bumps one global epoch;
//!   the next VM poll on each thread notices the bump and records the Raku
//!   stack it is standing on. Times are statistical and never asserted.
//!
//! Everything here is reached only from inside the armed branch of
//! [`crate::vm::vm_poll`], so a run that never profiles does not execute a
//! single instruction of it (ADR-0106 §8 gates 1/1b/1c).

pub(crate) mod aggregate;
pub(crate) mod counts;
pub(crate) mod paths;
pub(crate) mod report;
pub(crate) mod sampler;

use crate::symbol::Symbol;

/// A `(file, line)` pair, the unit both halves of the profile attribute to.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct LineLocation {
    pub(crate) file: Symbol,
    pub(crate) line: u32,
}

/// A routine's identity: its package, its short name, and the file its body
/// was declared in (`None` for the mainline or a native routine).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RoutineLocation {
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
    pub(crate) file: Option<Symbol>,
}

/// One caller's edge into a routine — NYTProf's most useful column, and the
/// one a flat line table cannot produce (ADR-0106 D3).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallsiteLocation {
    pub(crate) caller_file: Symbol,
    pub(crate) caller_line: u32,
    pub(crate) package: Symbol,
    pub(crate) name: Symbol,
}

pub(crate) use counts::{record_line_at, record_routine_frame};
pub(crate) use report::flush_at_exit;
pub(crate) use sampler::{arm, exclude_non_raku, sample_if_due};
