//! A routine's native int kernel state (ADR-19288), carried in its
//! `CompiledCode`'s JIT state. The kernel itself is compiled and entered by
//! `vm::vm_kernel`; this leaf only holds what it leaves behind.

use crate::symbol::Symbol;
use std::sync::atomic::{AtomicBool, AtomicU32};

/// A compiled kernel.
#[derive(Debug, Clone, Copy)]
pub(crate) struct KernelInfo {
    /// The native entry point.
    pub(crate) entry: usize,
    /// Its parameter count.
    pub(crate) arity: u8,
    /// The name every self-call in the body uses; `None` when there is none.
    pub(crate) self_name: Option<Symbol>,
}

/// One routine's kernel state.
#[derive(Debug, Default)]
pub(crate) struct KernelSlot {
    /// Calls seen while cold.
    pub(crate) calls: AtomicU32,
    /// `Some(None)`: declined for good. `Some(Some(_))`: compiled.
    pub(crate) info: std::sync::OnceLock<Option<KernelInfo>>,
    /// Consecutive entries that bailed.
    pub(crate) bail_streak: AtomicU32,
    /// Set after too many consecutive bails: never entered again.
    pub(crate) disabled: AtomicBool,
}
