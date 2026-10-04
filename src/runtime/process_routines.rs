//! Routine symbols in the `PROCESS::` stash (`PROCESS::<&chdir>`, #9881).
//!
//! A stash entry for a `&` symbol holds what it was given. Bound with `:=`
//! (`PROCESS::<&f> := sub {...}`), or installed by the setting (rakudo's
//! process-level `&*chdir`), it is the code object itself, with no container,
//! so an assignment to it dies with "Cannot assign to an immutable value". An
//! assignment to a key nobody bound creates a Scalar for it, as for any other
//! stash key, and later assignments store into that Scalar.
//!
//! Either way the entry is the process-level dynamic `&*name`: it lives in the
//! process stash (ADR-11318) under that spelling, so `&*name` and
//! `PROCESS::<&name>` name one binding.

use super::*;

/// The routines the process stash holds before anything is written to it,
/// resolved through the dynamic `&*name` lookup.
pub(crate) const PROCESS_ROUTINES: &[&str] = &["chdir"];

impl Interpreter {
    /// `PROCESS::<&name> = value` (or `:=`, when `value` carries the bind
    /// marker): store the routine symbol `name` in the process stash.
    // Cost: O(|name|), a stash probe and a stash write.
    pub(crate) fn store_process_routine(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let key = format!("&*{name}");
        let is_bind = Self::is_bind_index_value(&value);
        let (value, _) = Self::unwrap_bind_index_value(value);
        let stored = if is_bind {
            value
        } else {
            match self.process_routine_binding(name, &key) {
                // A bound code object is not a container.
                Some(existing) if !existing.is_container_ref() => {
                    return Err(RuntimeError::immutable_value());
                }
                Some(cell) => {
                    if let ValueView::ContainerRef(cell) = cell.view() {
                        Value::store_through_cell(&cell, &value);
                    }
                    cell
                }
                None => value.into_container_ref(),
            }
        };
        if self.publish_process_dynamic(&key, stored.clone()) {
            self.env_mut().insert(key, stored.clone());
        }
        Ok(stored.into_deref())
    }

    /// The current process-level binding of the routine symbol `name` (env
    /// spelling `key`): a published one, else a setting-installed routine.
    // Cost: O(|key|), one stash probe; a setting routine adds one code-var
    // resolution.
    fn process_routine_binding(&self, name: &str, key: &str) -> Option<Value> {
        if let Some(published) = self.lexicals.process_dynamics.get(key) {
            return Some(published);
        }
        PROCESS_ROUTINES
            .contains(&name)
            .then(|| self.resolve_code_var(&format!("*{name}")))
            .filter(|routine| !routine.is_nil())
    }
}
