//! The emitter's dynamic scope, handed to the handler a live `emit` runs.
//!
//! Rakudo runs a `whenever` body on the *emitting* thread, inside the
//! `Supplier.emit` call, so a dynamic variable read in the body resolves
//! through the emitter's callers (`my $*DB = ...; $supplier.emit(...)` is seen
//! by the handler), not through whoever created the `react`. mutsu runs the
//! handler on the react's own interpreter while the emitter waits for it
//! (`ReactWaker::begin_synchronous_delivery`), so the emitter publishes a
//! snapshot of its dynamic variables under the event's sequence number and the
//! drive loop overlays it for the duration of the handler.
//!
//! The snapshot is only taken when a listener could consume the event
//! synchronously (`supplier_has_listeners`), so an `emit` with nobody
//! listening pays nothing.
use super::super::Interpreter;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// A snapshot of the dynamic variables (`$*x`, `@*x`, ...) of one thread.
pub(crate) type DynScope = Arc<Vec<(String, Value)>>;

static EMIT_SCOPES: Mutex<Option<HashMap<u64, DynScope>>> = Mutex::new(None);

thread_local! {
    /// The scope the next `supplier_emit_push` on this thread publishes.
    static PENDING: RefCell<Option<DynScope>> = const { RefCell::new(None) };
}

/// Stage `scope` for the next emission pushed from this thread.
// Cost: O(1).
pub(crate) fn stage(scope: DynScope) {
    PENDING.with(|p| *p.borrow_mut() = Some(scope));
}

/// Publish the staged scope (if any) for the event sequenced at `seq`;
/// otherwise drop a stale one. Called with the supplier registry lock held.
// Cost: O(1) amortized.
pub(crate) fn publish(seq: u64) {
    if let Some(scope) = PENDING.with(|p| p.borrow_mut().take())
        && let Ok(mut map) = EMIT_SCOPES.lock()
    {
        map.get_or_insert_with(HashMap::new).insert(seq, scope);
    }
}

/// Forget whatever is staged on this thread.
// Cost: O(1).
pub(crate) fn unstage() {
    PENDING.with(|p| p.borrow_mut().take());
}

/// The scope the emitter published for the event sequenced at `seq`.
// Cost: O(1) amortized.
pub(crate) fn lookup(seq: u64) -> Option<DynScope> {
    EMIT_SCOPES
        .lock()
        .ok()
        .and_then(|map| map.as_ref().and_then(|m| m.get(&seq).cloned()))
}

/// The emitter is done waiting for `seq`: its scope is no longer needed.
// Cost: O(1) amortized.
pub(crate) fn discard(seq: u64) {
    if let Ok(mut map) = EMIT_SCOPES.lock()
        && let Some(m) = map.as_mut()
    {
        m.remove(&seq);
    }
}

/// Env keys that are internal bookkeeping rather than user-visible dynamics.
fn is_user_dynamic(key: &str) -> bool {
    key.trim_start_matches(['$', '@', '%', '&']).starts_with('*') && !key.contains("__")
}

impl Interpreter {
    /// Snapshot this interpreter's dynamic variables for [`stage`].
    // Cost: O(e), e = env entries.
    pub(crate) fn dynamic_scope_snapshot(&self) -> DynScope {
        // A scoped (multi-tier) env iterates its overlay only.
        let flat;
        let env = if self.env.is_scoped() {
            flat = self.env.flattened();
            &flat
        } else {
            &self.env
        };
        Arc::new(
            env.iter()
                .filter_map(|(k, v)| {
                    k.with_str(|s| is_user_dynamic(s).then(|| (s.to_string(), v.clone())))
                })
                .collect(),
        )
    }

    /// Bind `scope` over this env, returning the bindings it shadowed for
    /// [`Self::leave_emitter_scope`].
    // Cost: O(d), d = variables in `scope`.
    pub(crate) fn enter_emitter_scope(&mut self, scope: &DynScope) -> Vec<(String, Option<Value>)> {
        let mut saved = Vec::with_capacity(scope.len());
        for (key, value) in scope.iter() {
            saved.push((key.clone(), self.env.get(key).cloned()));
            self.env.insert(key.clone(), value.clone());
        }
        saved
    }

    /// Put back what [`Self::enter_emitter_scope`] shadowed.
    // Cost: O(d), d = saved bindings.
    pub(crate) fn leave_emitter_scope(&mut self, saved: Vec<(String, Option<Value>)>) {
        for (key, old) in saved {
            match old {
                Some(value) => {
                    self.env.insert(key, value);
                }
                None => {
                    self.env.remove(&key);
                }
            }
        }
    }
}
