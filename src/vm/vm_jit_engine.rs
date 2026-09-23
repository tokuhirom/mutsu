//! The one process-wide Cranelift module, shared by the bytecode JIT
//! (ADR-0004) and the TRIR lowering (ADR-0116).
//!
//! Compiled code lives for the process lifetime (entries are cached as raw
//! function pointers), so the module is created once and never dropped.

use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use std::sync::{Mutex, MutexGuard};

pub(crate) struct Engine {
    pub(crate) module: JITModule,
    /// Monotonic counter naming each defined function.
    pub(crate) fn_counter: u64,
}

// SAFETY: `JITModule` is only manipulated under the `ENGINE` mutex; the
// finalized code memory it owns is immutable after `finalize_definitions`
// and is executed (not mutated) from any thread, which is sound regardless
// of which thread performed the compilation.
unsafe impl Send for Engine {}

static ENGINE: Mutex<Option<Engine>> = Mutex::new(None);

/// Lock the engine slot. A poisoned lock is taken over: the module is only
/// ever appended to, so a panic mid-compile leaves at worst an unused
/// declaration behind.
pub(crate) fn lock() -> MutexGuard<'static, Option<Engine>> {
    ENGINE.lock().unwrap_or_else(|e| e.into_inner())
}

/// The engine behind `guard`, created on first use. `None` when this host
/// has no usable Cranelift ISA.
pub(crate) fn get_or_init<'a>(
    guard: &'a mut MutexGuard<'static, Option<Engine>>,
) -> Option<&'a mut Engine> {
    if guard.is_none() {
        let mut flags = settings::builder();
        flags.set("opt_level", "speed").ok()?;
        let isa = cranelift_native::builder()
            .ok()?
            .finish(settings::Flags::new(flags))
            .ok()?;
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        **guard = Some(Engine {
            module: JITModule::new(builder),
            fn_counter: 0,
        });
    }
    guard.as_mut()
}
