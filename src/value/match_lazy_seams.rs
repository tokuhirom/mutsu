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
                    None => slot.subcap.as_ref().is_some_and(|sc| group_may_dispatch(sc)),
                }
        })
    }
}
