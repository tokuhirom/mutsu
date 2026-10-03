//! The `guards` subsystem of ADR-10779: the recursion guards that keep a
//! `.raku`/`.gist` render of a self-referencing structure from looping.
//!
//! Both guards work the same way. A render registers the id it is rendering;
//! meeting that id again while it is still active means a reference cycle, so
//! the inner occurrence renders a backreference name instead of recursing, and
//! the outer occurrence, on leaving, learns that it must wrap its rendering in
//! the `(my \NAME = ...)` binding the backreference names.

use std::collections::HashSet;
use std::hash::Hash;

/// The ids currently being rendered, and the ones a cycle backreference was
/// emitted for during that render.
#[derive(Debug)]
pub(crate) struct CycleGuard<K> {
    active: Vec<K>,
    cycle_hit: HashSet<K>,
}

impl<K> Default for CycleGuard<K> {
    fn default() -> Self {
        Self {
            active: Vec::new(),
            cycle_hit: HashSet::new(),
        }
    }
}

impl<K: Eq + Hash + Clone> CycleGuard<K> {
    /// Whether `key` is already being rendered. If it is, the cycle is
    /// recorded, so the outer render's [`Self::leave`] reports it, and the
    /// caller renders a backreference instead of recursing.
    // Cost: O(d), d = nesting depth of the renders in progress.
    pub(crate) fn revisit(&mut self, key: &K) -> bool {
        if self.active.contains(key) {
            self.cycle_hit.insert(key.clone());
            true
        } else {
            false
        }
    }

    /// Start rendering `key`.
    // Cost: O(1) amortized.
    pub(crate) fn enter(&mut self, key: K) {
        self.active.push(key);
    }

    /// Finish rendering `key`. Returns whether a backreference to it was
    /// emitted inside the render, i.e. whether the result needs the
    /// `(my \NAME = ...)` wrapper.
    // Cost: O(d), d = nesting depth of the renders in progress.
    pub(crate) fn leave(&mut self, key: &K) -> bool {
        if let Some(pos) = self.active.iter().rposition(|x| x == key) {
            self.active.remove(pos);
        }
        self.cycle_hit.remove(key)
    }
}

/// The interpreter's two render guards.
#[derive(Debug, Default)]
pub(crate) struct RakuCycleGuards {
    /// `Mu.rakuseen($id, &code)`, the guard a user-written `.raku`/`.gist`
    /// wraps its body in. Keyed by the id string the caller passes.
    pub(crate) rakuseen: CycleGuard<String>,
    /// The native `.raku` of instances: the nested-leaf walker
    /// (`methods_raku_dispatch`) and the instance renderer
    /// (`methods_instance_ops`). Keyed by instance id, so a self-referencing
    /// object (`$obj.myself[0] = $obj`) does not recurse through its own
    /// attribute container.
    pub(crate) leaf: CycleGuard<u64>,
}

impl RakuCycleGuards {
    /// A spawned thread starts with no render in progress.
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use super::CycleGuard;

    #[test]
    fn a_revisit_inside_a_render_marks_the_outer_render() {
        let mut g = CycleGuard::<u64>::default();
        assert!(!g.revisit(&1));
        g.enter(1);
        g.enter(2);
        assert!(g.revisit(&1));
        assert!(!g.leave(&2));
        assert!(g.leave(&1));
        // The hit was consumed: a later render of the same id starts clean.
        g.enter(1);
        assert!(!g.leave(&1));
    }

    #[test]
    fn leaving_without_a_cycle_reports_none() {
        let mut g = CycleGuard::<String>::default();
        g.enter("a".to_string());
        assert!(!g.revisit(&"b".to_string()));
        assert!(!g.leave(&"a".to_string()));
        assert!(!g.revisit(&"a".to_string()));
    }
}
