//! Parse-time export probe (ADR-0026 §2.1 amendment for #9500).
//!
//! Some modules compute the *names* they export at run time:
//!
//! ```raku
//! my package EXPORT::ALL {
//!     for %currencies.keys -> $code {
//!         OUR::{'&postfix:<' ~ $code ~ '>'} := sub ($n) { ... };
//!     }
//! }
//! ```
//!
//! Rakudo runs a module when its `use` is compiled, so the importer's parse
//! already sees `&postfix:<USD>` and `5USD` parses as a postfix call. No static
//! scan of the module source can list those names, so when the parser's module
//! scan finds a binding like this (a non-literal key into an export stash,
//! `ModuleScanResult::dynamic_export_stash`) the parser runs the module here —
//! a fresh [`Interpreter`] on a fresh thread, exactly like slang activation —
//! and reads back every routine the load exported.
//!
//! The probe only *adds* parser knowledge. The program's own run-time `use`
//! still loads the module in the real interpreter, so a module that dies while
//! loading reports that error there, at its real location.

use super::*;
use std::cell::RefCell;

/// The thread name marks a probe sub-interpreter.
const PROBE_THREAD_NAME: &str = "mutsu-export-probe";

thread_local! {
    /// Modules whose probe is running on this thread or one of its ancestors.
    /// A probe's own parse may `use` another dynamic-export module and probe
    /// it in turn (legitimately: that module's operators may appear in the
    /// probed module's source), but a `use` cycle between two such modules
    /// must not spawn probe threads forever.
    static PROBE_CHAIN: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
}

/// Run `use <module>` in a fresh interpreter on a fresh thread and return the
/// names (sigil-less, e.g. `postfix:<USD>`) of every routine the module's load
/// registered as exported, under any tag. `lib_paths` is the parser's current
/// module search path list. Returns an empty list when the module is already
/// being probed further up this chain.
///
/// Spawned via `try_spawn_user_thread` for the same reasons as
/// [`super::slang_activation::run_slang_activation`]: the closure runs user
/// code, so it must be a registered GC mutator with a user-code-sized stack,
/// and the parent waits in `gc::block_quiescent`.
pub(crate) fn probe_module_exports(
    module: String,
    lib_paths: Vec<String>,
) -> Result<Vec<String>, String> {
    let mut chain = PROBE_CHAIN.with(|c| c.borrow().clone());
    if chain.contains(&module) {
        return Ok(Vec::new());
    }
    chain.push(module.clone());
    let handle = crate::runtime::builtins_system::try_spawn_user_thread(
        PROBE_THREAD_NAME,
        crate::runtime::builtins_system::StackPolicy::Required,
        move || -> Result<Vec<String>, String> {
            PROBE_CHAIN.with(|c| *c.borrow_mut() = chain);
            let mut interp = Interpreter::new();
            for path in lib_paths {
                interp.add_lib_path(path);
            }
            interp
                .use_module(&module)
                .map_err(|e| e.message.to_string())?;
            Ok(interp.module_exported_routine_names(&module))
        },
    )
    .map_err(|e| e.message())?;
    crate::gc::block_quiescent(|| handle.join())
        .map_err(|_| "export probe thread panicked".to_string())?
}

impl Interpreter {
    /// Every routine name `module` exports under any tag: its exported subs,
    /// plus the `&`-sigiled entries of its exported symbols (a closure bound
    /// into an export stash is recorded as an exported `&name` variable).
    /// Sorted and deduplicated.
    // Cost: O(s + v), s = exported subs of `module`, v = its exported symbols.
    pub(crate) fn module_exported_routine_names(&self, module: &str) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();
        if let Some(subs) = self.exported_subs.get(module) {
            names.extend(subs.keys().cloned());
        }
        if let Some(vars) = self.exported_vars.get(module) {
            names.extend(
                vars.keys()
                    .filter_map(|name| name.strip_prefix('&'))
                    .filter(|name| !name.is_empty())
                    .map(str::to_string),
            );
        }
        names.sort();
        names.dedup();
        names
    }
}
