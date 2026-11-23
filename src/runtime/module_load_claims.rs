//! Per-module load claims: a compunit loads once even when two threads ask for
//! it at the same moment (ADR-17216).
//!
//! The loaded-module table is world state, so a thread that `require`s a
//! module another thread finished loading sees it loaded. But the check and
//! the load were two steps, and two threads that both checked before either
//! finished both ran the module's body. Each load entry point now takes a
//! *claim* on the module's name first. A second thread waits until the first
//! thread's claim is released and then takes the already-loaded path.
//!
//! A claim is per module name, not one process-wide lock, so a module whose
//! mainline awaits a `start` that loads a *different* module still finishes
//! (rakudo runs that program too). A claim is re-entrant for the thread that
//! holds it: a module's nested `use` of itself is the existing
//! circular-dependency error, not a hang. Two threads whose loads need each
//! other's module (`A` holds `M` and waits for `N`, `B` holds `N` and waits
//! for `M`) would wait forever, so a thread whose wait would close such a
//! cycle does not wait and loads as it did before claims existed.

use std::collections::HashMap;
use std::sync::{Arc, Condvar, Mutex};
use std::thread::ThreadId;

#[derive(Default)]
struct ClaimState {
    /// Module name -> the thread loading it and its re-entry depth.
    holders: HashMap<String, (ThreadId, usize)>,
    /// Thread -> the module whose claim it is waiting for.
    waits_for: HashMap<ThreadId, String>,
}

/// The claims of one program world, shared by every thread
/// ([`crate::runtime::module_state::ModuleWorld`]).
#[derive(Default)]
pub(crate) struct ModuleLoadClaims {
    state: Mutex<ClaimState>,
    released: Condvar,
}

/// A held claim; dropping it releases the claim and wakes the waiters.
pub(crate) struct ModuleLoadClaim {
    claims: Arc<ModuleLoadClaims>,
    /// `None` when no claim was taken (the wait would have deadlocked).
    module: Option<String>,
}

impl ModuleLoadClaims {
    /// Take the claim on `module` for this thread, waiting while another
    /// thread holds it.
    // Cost: O(|module|) uncontended (one hash insert under the mutex); a
    // contended claim also walks the wait-for chain, O(t), t = waiting
    // threads, and then blocks until the holder's load finishes.
    pub(crate) fn claim(self: &Arc<Self>, module: &str) -> ModuleLoadClaim {
        let me = std::thread::current().id();
        let mut state = self.state.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        loop {
            let owner = match state.holders.get_mut(module) {
                None => {
                    state.holders.insert(module.to_string(), (me, 1));
                    return self.held(module);
                }
                Some((owner, depth)) if *owner == me => {
                    *depth += 1;
                    return self.held(module);
                }
                Some((owner, _)) => *owner,
            };
            if Self::wait_closes_cycle(&state, owner, me) {
                return ModuleLoadClaim {
                    claims: Arc::clone(self),
                    module: None,
                };
            }
            state.waits_for.insert(me, module.to_string());
            drop(state);
            let woken = crate::gc::wait_until(&self.state, &self.released, |s| {
                !s.holders.contains_key(module)
            });
            // Single-threaded wasm never has another holder to wait for.
            let Some(mut guard) = woken else {
                return ModuleLoadClaim {
                    claims: Arc::clone(self),
                    module: None,
                };
            };
            guard.waits_for.remove(&me);
            state = guard;
        }
    }

    fn held(self: &Arc<Self>, module: &str) -> ModuleLoadClaim {
        ModuleLoadClaim {
            claims: Arc::clone(self),
            module: Some(module.to_string()),
        }
    }

    /// Whether `me` waiting for `owner` would close a wait-for cycle: `owner`
    /// is (transitively) waiting for a module `me` holds.
    // Cost: O(t), t = threads in the wait-for map (each step follows one edge).
    fn wait_closes_cycle(state: &ClaimState, owner: ThreadId, me: ThreadId) -> bool {
        let mut current = owner;
        for _ in 0..=state.waits_for.len() {
            if current == me {
                return true;
            }
            let Some(next) = state
                .waits_for
                .get(&current)
                .and_then(|module| state.holders.get(module))
            else {
                return false;
            };
            current = next.0;
        }
        false
    }
}

impl Drop for ModuleLoadClaim {
    fn drop(&mut self) {
        let Some(module) = self.module.take() else {
            return;
        };
        let mut state = self.claims.state.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some((_, depth)) = state.holders.get_mut(&module) {
            *depth -= 1;
            if *depth == 0 {
                state.holders.remove(&module);
                drop(state);
                self.claims.released.notify_all();
            }
        }
    }
}

impl crate::runtime::Interpreter {
    /// Claim `module` for a load on this thread; see the module docs.
    // Cost: see [`ModuleLoadClaims::claim`].
    pub(crate) fn claim_module_load(&self, module: &str) -> ModuleLoadClaim {
        self.module.world.load_claims.claim(module)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn claim_is_reentrant_and_released() {
        let claims = Arc::new(ModuleLoadClaims::default());
        let outer = claims.claim("M");
        let inner = claims.claim("M");
        drop(inner);
        assert!(claims.state.lock().unwrap().holders.contains_key("M"));
        drop(outer);
        assert!(claims.state.lock().unwrap().holders.is_empty());
    }

    #[test]
    fn second_thread_waits_for_the_first_load() {
        let claims = Arc::new(ModuleLoadClaims::default());
        let order = Arc::new(Mutex::new(Vec::new()));
        let first = claims.claim("M");
        let (c2, o2) = (Arc::clone(&claims), Arc::clone(&order));
        let waiter = std::thread::spawn(move || {
            let _claim = c2.claim("M");
            o2.lock().unwrap().push("second");
        });
        std::thread::sleep(std::time::Duration::from_millis(50));
        order.lock().unwrap().push("first");
        drop(first);
        waiter.join().unwrap();
        assert_eq!(*order.lock().unwrap(), vec!["first", "second"]);
    }
}
