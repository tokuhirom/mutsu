//! The interpreter's side of the compile-time loader seam (ADR-17558): a `use`
//! loads its module when the parser reaches it, in the interpreter that will
//! run the unit, and the unit's run-time `use` then only imports.
//!
//! The load is the BEGIN-time preload the prologue performs for a nested `use`
//! (`OpCode::PreloadModule`, `Interpreter::preload_module`), moved to the
//! moment the parser finishes the statement: the module's packages, types and
//! routines are registered and its mainline runs, nothing is imported. The
//! `use` that later runs in position finds the module in `loaded_modules` and
//! takes the import-only path. A module's `sub EXPORT` hook (its own, or the
//! `&EXPORT` it imported, the Slangify pattern) is only recorded by the load:
//! the import runs it, once per `use`, as rakudo does.

use super::*;
use crate::compile_time_loader::{CompileTimeLoader, LoadOutcome, LoaderGuard, UseRequest};

impl CompileTimeLoader for Interpreter {
    // Cost: O(p) for a module already loaded or not taken, p = search path
    // entries; the module's own load otherwise.
    fn load_use(&mut self, request: &UseRequest<'_>) -> LoadOutcome {
        // The slang activation interpreter (ADR-0026 §2.1) keeps loading at
        // run time: which `define_slang` it hands to the importer is decided by
        // the load order of the run-time `use`s (`slang_define_slang`). Slice 4
        // of ADR-17558 retires that interpreter.
        if std::thread::current().name()
            == Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
        {
            return LoadOutcome::NotTaken;
        }
        // The same `use`s the prologue preloads: not a pragma, `Test`, a
        // `:from<...>` compunit, a `:if` load or one whose arguments feed a
        // `sub EXPORT`.
        let Some(module) = crate::compiler::begin_use::preloadable_use_shape(
            request.module,
            request.tags,
            request.has_condition,
            request.has_arg,
        ) else {
            return LoadOutcome::NotTaken;
        };
        // A module that imports through a `sub EXPORT` hook runs the hook once
        // per `use`, at its import, with the `use`'s arguments.
        // TODO: run the hook at parse time and install its map at the import
        // (ADR-17558 slice 2); until then such a module loads at run time,
        // where the first load and the hook happen together.
        if request.through_hook {
            return LoadOutcome::NotTaken;
        }
        let (bare, _) = Self::split_dist_selectors(module);
        if self.module.loaded_modules.contains(bare) {
            return LoadOutcome::Loaded;
        }
        // The unit's parse-time `use lib` additions are BEGIN-time effects that
        // precede this `use`: the module resolves through them, prepended as a
        // `use lib` prepends. They are on the chain only for this load; the
        // BEGIN-time prologue registers them for good (and in `$*REPO`), at
        // their own positions.
        let known = self.parser_scan_lib_paths();
        let saved_lib_paths = std::sync::Arc::clone(&self.module.lib_paths);
        for path in request.lib_paths.iter().filter(|path| !known.contains(path)) {
            self.prepend_lib_path(Self::strip_file_repo_spec(path.clone()));
        }
        // The module's own compile is a parse of its own: the importer's parse
        // is suspended around it and resumes exactly where it was.
        let suspended = crate::parser::unit_state::suspend();
        let result = if request.nested {
            // A `use` inside a block: what the module installs while it loads
            // is lexical to the block (`OpCode::PreloadModule`'s scope).
            self.push_preload_scope();
            let result = self.without_use_attach_depth(|interp| interp.preload_module(module));
            self.pop_import_scope();
            result
        } else {
            self.without_use_attach_depth(|interp| interp.preload_module(module))
        };
        drop(suspended);
        self.module.lib_paths = saved_lib_paths;
        match result {
            Ok(()) => LoadOutcome::Loaded,
            // A module the chain cannot find now may still be found when the
            // unit runs (a `use lib` the parser could not fold, a repository a
            // BEGIN registers): the in-position `use` loads it, or reports it.
            Err(err) if err.is_unsatisfied_dependency() => LoadOutcome::NotTaken,
            Err(err) => LoadOutcome::Failed(err),
        }
    }
}

impl Interpreter {
    /// Run `parse`, the parse of a unit this interpreter is about to run, with
    /// this interpreter as its compile-time loader. A parse that failed because
    /// a `use` failed to load reports the load's error.
    // Cost: O(1) besides `parse`.
    pub(crate) fn parse_with_loader<T>(
        &mut self,
        parse: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        let loader: *mut dyn CompileTimeLoader = self as *mut Self;
        crate::compile_time_loader::take_failure();
        // SAFETY: `loader` comes from the live `&mut self`, which this function
        // holds for the guard's whole lifetime and does not use until the
        // guard is dropped: `parse` cannot capture `self` (it is mutably
        // borrowed by this call), so only `compile_time_loader::load_use`
        // re-borrows the interpreter while the guard lives.
        let guard = unsafe { LoaderGuard::push(loader) };
        let result = parse();
        drop(guard);
        let failure = crate::compile_time_loader::take_failure();
        match (result, failure) {
            (Err(_), Some(failure)) => Err(failure),
            (result, _) => result,
        }
    }
}
