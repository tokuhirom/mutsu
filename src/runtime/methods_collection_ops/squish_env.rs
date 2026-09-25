//! The env snapshot `.squish(:as/:with)` reverts its eager pass with (#9162).
//!
//! `dispatch_squish` runs the callbacks eagerly, then reverts their side
//! effects on the env so the lazy iterator's re-run does not apply them twice
//! (see the revert in `dispatch_iterator_method`). The snapshot used to detach
//! -- copy -- every Array/Hash lexical in scope, so a small squish next to a
//! large array paid for the large array. Only the containers a callback can
//! name are detached now; every other binding is compared by identity.

use super::*;
use crate::symbol::Symbol;

/// The env as it was before the callbacks ran.
pub(in crate::runtime) struct SquishEnvSnapshot {
    /// A shared (copy-on-write) handle on the whole env: an O(1) clone that
    /// stays pointer-equal to the live env for as long as no binding changes.
    before: crate::env::Env,
    /// Detached contents of the Array/Hash lexicals the callbacks can name
    /// (every Array/Hash in the env, for a callback whose free variables are
    /// unknown).
    watched: Vec<(Symbol, Value)>,
}

/// The Array/Hash behind an env binding, looking through a `ContainerRef`
/// cell (ADR-0055: an escaping capture the creating frame cannot vouch for;
/// the binding never changes -- the cell IS the binding -- so its CONTENTS
/// are what is snapshotted, and the revert writes them back THROUGH the
/// cell).
fn container_contents(v: &Value) -> Option<Value> {
    match v.view() {
        ValueView::Array(..) | ValueView::Hash(..) => Some(v.clone()),
        ValueView::ContainerRef(_) => {
            let inner = v.deref_container();
            matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(..)).then_some(inner)
        }
        _ => None,
    }
}

impl Interpreter {
    /// Snapshot the env before running the squish `callbacks`.
    ///
    /// A callback can only push into a container it can name, i.e. one of its
    /// free variables, so only those are detached. A callback that is not a
    /// compiled closure (a builtin routine) names nothing; a closure whose
    /// free-variable set is unknown falls back to detaching every Array/Hash
    /// in scope.
    // Cost: O(f + A_f), f = free variables of the callbacks, A_f = elements of
    // the Array/Hash lexicals among them; O(V + A) in the unknown-free-variable
    // fallback, V = env entries, A = elements of every Array/Hash in scope.
    pub(in crate::runtime) fn squish_env_snapshot(
        &self,
        callbacks: &[Option<&Value>],
    ) -> SquishEnvSnapshot {
        let mut names: Vec<Symbol> = Vec::new();
        let mut unknown = false;
        for callback in callbacks.iter().flatten() {
            match callback.view() {
                ValueView::Sub(data) => match data.compiled_code.as_ref() {
                    Some(cc) => names.extend(cc.capture_free_var_set().iter().copied()),
                    None => unknown = true,
                },
                ValueView::WeakSub(_) => unknown = true,
                _ => {}
            }
        }
        let watched = if unknown {
            self.env
                .iter()
                .filter_map(|(k, v)| Some((*k, container_contents(v)?.detach_shared_container())))
                .collect()
        } else {
            names.sort_unstable();
            names.dedup();
            names
                .into_iter()
                .filter_map(|k| {
                    let v = self.env.get_sym(k)?;
                    Some((k, container_contents(v)?.detach_shared_container()))
                })
                .collect()
        };
        SquishEnvSnapshot {
            before: self.env.clone(),
            watched,
        }
    }

    /// What the eager pass changed, as the revert `dispatch_iterator_method`
    /// applies: the watched containers whose contents differ, and -- only
    /// when some binding itself changed (the env is no longer pointer-equal
    /// to the snapshot) -- every rebound or newly added name.
    // Cost: O(w + A_w), w = watched containers, A_w = their elements; plus
    // O(V), V = env entries, when a callback rebound a lexical.
    pub(in crate::runtime) fn squish_env_diff(
        &self,
        snapshot: SquishEnvSnapshot,
        revert_values: &mut ValueMap,
        revert_remove: &mut Vec<String>,
    ) {
        let SquishEnvSnapshot { before, watched } = snapshot;
        let live_contents = |k: Symbol| {
            self.env.get_sym(k).map(|v| match v.view() {
                // Compare through a cell: the snapshot holds the detached
                // CONTENTS of a `ContainerRef` binding.
                ValueView::ContainerRef(_) => v.deref_container(),
                _ => v.clone(),
            })
        };
        for (k, old_v) in &watched {
            if live_contents(*k).as_ref() != Some(old_v) {
                revert_values.insert(k.resolve(), old_v.clone());
            }
        }
        if self.env.ptr_eq(&before) {
            return;
        }
        for (k, old_v) in before.iter() {
            if watched.iter().any(|(w, _)| w == k) {
                continue;
            }
            let old_v = match old_v.view() {
                ValueView::ContainerRef(_) => old_v.deref_container(),
                _ => old_v.clone(),
            };
            if live_contents(*k).as_ref() != Some(&old_v) {
                revert_values.insert(k.resolve(), old_v);
            }
        }
        for k in self.env.keys() {
            if !before.contains_key_sym(*k) {
                revert_remove.push(k.resolve());
            }
        }
    }
}
