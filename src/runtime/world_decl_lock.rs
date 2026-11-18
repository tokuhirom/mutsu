//! Per-declaration-site locks for type declarations (ADR-17216 slice 2b).
//!
//! The type registry is shared by every thread, and registering a class or a
//! role is many registry writes plus checks between them: "has this
//! declaration site already registered its class?" (a `class :: { ... }`
//! evaluated on every call of a routine), the method table filled method by
//! method. Two threads running the same declaration at once would see each
//! other's half (Red's `grep` declares `my $*OUT = class :: { method put ... }`
//! on every call; two concurrent `grep`s reported "already has a method
//! 'put'").
//!
//! So a declaration's registration runs under a lock keyed by its
//! declaration site: the same site on two threads is serialised, different
//! sites are not. Rakudo serialises composition per type the same way (the
//! metamodel's per-HOW lock), not with one process-wide lock.
//!
//! The lock is held while the declaration runs user code (traits, a class
//! body). A process-wide lock would turn any user lock taken in that code
//! into a lock-order inversion with every other thread that declares a type
//! while holding the same user lock; a per-site lock can only invert with a
//! thread running the same declaration. It is reentrant on the holding
//! thread: a declaration evaluated again from its own body (a trait that
//! re-runs the routine) only deepens it.

use std::collections::HashMap;
use std::sync::{Condvar, Mutex, PoisonError};
use std::thread::ThreadId;

/// The held sites: holding thread and nesting depth per site key.
static HELD: Mutex<Option<HashMap<usize, (ThreadId, usize)>>> = Mutex::new(None);
static RELEASED: Condvar = Condvar::new();

/// Held while the declaration at one site registers; see the module docs.
pub(crate) struct DeclLockGuard {
    site: usize,
}

/// Take the lock of declaration site `site` (any value that identifies the
/// site for as long as its code is alive, such as the address of its plan),
/// waiting while another thread registers the same site.
// Cost: O(1) expected uncontended (one mutex acquisition and a hash probe);
// otherwise the wait.
pub(crate) fn enter(site: usize) -> DeclLockGuard {
    let me = std::thread::current().id();
    let mut held = HELD.lock().unwrap_or_else(PoisonError::into_inner);
    loop {
        let table = held.get_or_insert_with(HashMap::new);
        match table.get_mut(&site) {
            None => {
                table.insert(site, (me, 1));
                break;
            }
            Some((thread, depth)) if *thread == me => {
                *depth += 1;
                break;
            }
            Some(_) => {
                held = RELEASED.wait(held).unwrap_or_else(PoisonError::into_inner);
            }
        }
    }
    DeclLockGuard { site }
}

impl Drop for DeclLockGuard {
    fn drop(&mut self) {
        let mut held = HELD.lock().unwrap_or_else(PoisonError::into_inner);
        let Some(table) = held.as_mut() else {
            return;
        };
        if let Some((_, depth)) = table.get_mut(&self.site) {
            *depth = depth.saturating_sub(1);
            if *depth == 0 {
                table.remove(&self.site);
                RELEASED.notify_all();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::enter;

    #[test]
    fn a_site_is_reentrant_and_other_sites_do_not_wait() {
        let outer = enter(0x1000);
        let nested = enter(0x1000);
        // Another site on another thread is not held up by this one.
        std::thread::spawn(|| drop(enter(0x2000))).join().unwrap();
        drop(nested);
        drop(outer);
        // Released: another thread can take the same site.
        std::thread::spawn(|| drop(enter(0x1000))).join().unwrap();
    }
}
