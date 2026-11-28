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

/// The machine kind of a loop kernel slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SlotKind {
    /// A plain `Int` held as an `i64`.
    Int,
    /// A `Num` held as an `f64`.
    Num,
}

/// A compiled loop kernel (ADR-19288 D5), specialized to the kinds its carried
/// slots held at entry.
#[derive(Debug)]
pub(crate) struct LoopKernelInfo {
    /// The native entry point: `extern "C" fn(*mut LoopCtx, *mut u64) -> u32`.
    pub(crate) entry: usize,
    /// The enclosing frame's slots the loop reads and writes, in buffer order
    /// after the counter words.
    pub(crate) carried: Box<[(u32, SlotKind)]>,
    /// The body's `my` declarations `(SetVarDynamic ip, slot, kind)`, replayed
    /// with their final values when the kernel finishes the loop.
    pub(crate) decls: Box<[(u32, u32, SlotKind)]>,
    /// `CheckReadOnly` / `CheckReadOnlyModify` name operands of the region,
    /// checked once at entry (`true` = the modify form).
    pub(crate) readonly_checks: Box<[(bool, u32)]>,
}

/// A loop site's variant: the carried kinds it was compiled for, and the
/// kernel (`None` when that variant declined).
pub(crate) type LoopVariant = (Box<[SlotKind]>, Option<std::sync::Arc<LoopKernelInfo>>);

/// One loop op's kernel state.
#[derive(Debug)]
pub(crate) struct LoopSite {
    /// The `ForLoop` / `WhileLoop` op's index.
    pub(crate) ip: u32,
    /// Entries seen (the hotness counter).
    pub(crate) entries: u32,
    /// The carried slots of the region, or `None` when the region can never
    /// be a kernel.
    pub(crate) carried: Option<std::sync::Arc<[u32]>>,
    /// Compiled (or declined) variants, keyed by the carried slots' kinds.
    pub(crate) variants: Vec<LoopVariant>,
}

/// Every loop site of one `CompiledCode`.
#[derive(Debug, Default)]
pub(crate) struct LoopKernelTable {
    pub(crate) sites: std::sync::Mutex<Vec<LoopSite>>,
}
