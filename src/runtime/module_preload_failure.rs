//! The error of a failed BEGIN-time module preload (#11351).
//!
//! A `use` inside a nested block (`our module M { use Dep; }`) is compiled to
//! a `PreloadModule` at the head of the unit plus the in-place `UseModule`.
//! The preload's error is not raised where it happens: the preload runs
//! before the code ahead of the block, which a load may depend on (a
//! `use lib` inside the block, say), and the in-place `use` loads the module
//! again in its proper position.
//!
//! When the module body itself died, though, the preload leaves behind
//! whatever the body registered before it died. The in-place reload then
//! fails on those leftovers (`Redeclaration of routine 'f'`) and hides the
//! real error. So the preload's error is kept, and the in-place `use`
//! reports it if it fails too: the first failure is the one that ran
//! against a clean registry. A module the preload could not find
//! (`X::CompUnit::UnsatisfiedDependency`) left nothing behind and is not
//! kept: the in-place `use` reports its own result for that.

use super::*;

impl Interpreter {
    /// Keep the error of a preload of `module` whose body died.
    // Cost: O(1) amortized (a copy-on-write table insert).
    pub(crate) fn note_failed_preload(&mut self, module: &str, err: &RuntimeError) {
        if err.is_unsatisfied_dependency() || self.module.loaded_modules.contains(module) {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module.module_visibility.failed_preloads)
            .entry(module.to_string())
            .or_insert_with(|| (err.message.to_string(), err.exception.as_deref().cloned()));
    }

    /// The error an in-place `use` of `module` reports when its load failed
    /// with `err`: the preload's, if the preload's module body died first.
    // Cost: O(1).
    pub(crate) fn in_place_use_error(&mut self, module: &str, err: RuntimeError) -> RuntimeError {
        if self.module.module_visibility.failed_preloads.is_empty() {
            return err;
        }
        match crate::runtime::cow_table_mut(&mut self.module.module_visibility.failed_preloads)
            .remove(module)
        {
            Some((message, exception)) => {
                let mut first = RuntimeError::new(message);
                first.exception = exception.map(Box::new);
                first
            }
            None => err,
        }
    }
}
