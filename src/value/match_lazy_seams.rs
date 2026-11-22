//! Non-forcing reads the end of a grammar parse makes on a lazy `Match`
//! (#18207): the named captures it installs as `$<name>`, and whether the
//! post-parse action walk has anything left to do below a node. Each answers
//! from the capture node, so the finished parse result is never materialized
//! just to be asked.

use super::match_lazy::group_may_dispatch;
use super::*;
use crate::value::regex_caps::SILENT_ACTION_MARKER_PREFIX;

impl Value {
    /// The named captures of an unforced lazy Match, as its `.hash` presents
    /// each slot, read from the capture node without materializing the
    /// Match. `None` for an eager or already-forced Match.
    // Cost: O(k), k = the node's named capture slots.
    pub(crate) fn match_lazy_named_values(&self) -> Option<Vec<(Symbol, Value)>> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        Some(
            node.cap
                .kids()
                .named
                .iter()
                .filter(|(key, _)| !key.starts_with(SILENT_ACTION_MARKER_PREFIX))
                .map(|(key, slot)| (*key, Value::named_slot_value(slot, &node.target)))
                .collect(),
        )
    }

    /// The class a lazy Match is an instance of (`Match`, or the grammar for
    /// a parse cursor), read from its node without materializing it; `None`
    /// for any other value.
    // Cost: O(1).
    pub(crate) fn match_lazy_class_sym(&self) -> Option<Symbol> {
        self.0.as_match_node().map(|node| node.cursor_class())
    }

    /// The named capture `key` of an unforced lazy Match, as its `.hash`
    /// presents the slot (`Nil` when there is none), read from the capture
    /// node without materializing the Match: `$<key>` in grammar-action code
    /// reads one per action (#12917). `None` for an eager or forced Match, and
    /// for a slot that reads as an Array (a quantified or repeated name): that
    /// Array is a container of the Match, and an action may mutate it
    /// (`shift @<operands>` in a loop), so every read must reach the same one
    /// -- the materialized `.hash` holds it, a node read would build a fresh
    /// copy each time.
    // Cost: O(k), k = the node's named capture slots (a short list).
    pub(crate) fn match_lazy_named_capture(&self, key: &str) -> Option<Value> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        // A name never interned was never captured. The silent-action marker
        // keys are not user-visible names, so a `$<key>` cannot reach them.
        let Some(sym) = Symbol::lookup(key) else {
            return Some(Value::NIL);
        };
        if sym.starts_with(SILENT_ACTION_MARKER_PREFIX) {
            return Some(Value::NIL);
        }
        Some(match node.cap.kids().named.get(&sym) {
            Some(slot) if slot.quantified || slot.nodes.len() != 1 => return None,
            Some(slot) => Value::named_slot_value(slot, &node.target),
            None => Value::NIL,
        })
    }

    /// The `:sym<...>` variant of an unforced lazy Match's node, as the walk
    /// node would report it; `None` for an eager or forced Match.
    // Cost: O(1).
    pub(crate) fn match_lazy_sym(&self) -> Option<Option<Symbol>> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        Some(node.cap.sym)
    }

    /// Whether an unforced lazy Match has nothing left for the action walk
    /// below it: every child capture node already ran its action at its
    /// reduction (or, for a positional group, has nothing to dispatch), and
    /// the node recorded no `:my $*x` values. `false` when unsure (an eager
    /// or forced Match). The answer is the one [`Self::match_action_walk_node`]
    /// would give through [`ActionWalkNode::has_no_children`], without
    /// building the node.
    // Cost: O(k), k = this node's capture slots; no allocation.
    pub(crate) fn match_lazy_walk_is_done(&self) -> bool {
        let Some(node) = self.0.as_match_node() else {
            return false;
        };
        if node.forced().is_some() {
            return false;
        }
        if node.cap.children.is_none() {
            return true;
        }
        let kids = node.cap.kids();
        if !kids.regex_vars.is_empty() {
            return false;
        }
        let named_pending = kids
            .named
            .iter()
            .any(|(_, slot)| slot.nodes.iter().any(|sc| !sc.reduced.get()));
        if named_pending {
            return false;
        }
        !kids.positional.iter().any(|slot| {
            !slot.nil
                && match &slot.quantified {
                    Some(entries) => entries
                        .iter()
                        .any(|(_, _, sc)| sc.as_ref().is_some_and(|sc| group_may_dispatch(sc))),
                    None => slot
                        .subcap
                        .as_ref()
                        .is_some_and(|sc| group_may_dispatch(sc)),
                }
        })
    }
}
