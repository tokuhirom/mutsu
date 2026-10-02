//! The capture node a sigil alias over a subrule call names.
//!
//! An alias on a call (`$<x>=<rule>`, `$<x>=<.rule>`, `@<x>=<&$re>`,
//! `$0=<rule>`) names the called rule's own Match -- its nested captures
//! included -- not a span-only leaf. The call has already filed that Match in
//! the store under its own key (or, for a silent call, under the hidden action
//! marker), so the alias reuses that node. Shared by the named (`$<x>=`) and
//! the numbered (`$N=`) alias, which file the node in different places.

use super::super::*;
use super::regex_trail::CapStore;
use std::sync::Arc;

impl Interpreter {
    /// The node a subrule call under `token`'s alias filed for the span
    /// `from..to`, and whether it was taken from a silent call's hidden action
    /// marker (which [`Interpreter::drop_reused_silent_marker`] then removes).
    /// `(None, _)` when `token`'s atom is not a call, or filed no such node.
    // Cost: O(1) hash lookups.
    pub(super) fn aliased_subrule_subcap(
        store: &CapStore,
        token: &RegexToken,
        from: usize,
        to: usize,
    ) -> (Option<Arc<CapNode>>, bool) {
        let RegexAtom::Named(atom_name) = &token.atom else {
            return (None, false);
        };
        let spec = atom_name.spec();
        let own_key = spec
            .capture_name
            .clone()
            .or_else(|| (!spec.silent).then(|| spec.lookup_name.clone()));
        let last_node = |sym: Symbol| store.caps().named.get(&sym)?.nodes.last().cloned();
        let mut reused_silent_marker = false;
        let node = own_key
            .and_then(|k| last_node(Symbol::intern(&k)))
            .or_else(|| {
                // A visible alias around a silent subrule (`$<x>=<.rule>`)
                // receives the subrule's capture under the hidden action
                // marker. Reuse that node so its nested captures and `.made`
                // value remain available to the alias instead of collapsing
                // it to a span-only leaf.
                let marker_node = last_node(spec.silent_marker_sym);
                reused_silent_marker = marker_node.is_some();
                marker_node
            })
            .filter(|sc| sc.from == from && sc.to == to);
        (node, reused_silent_marker)
    }

    /// Remove the hidden action-marker entry whose node
    /// [`Interpreter::aliased_subrule_subcap`] handed to the alias, so the
    /// silent call does not also surface under the marker.
    // Cost: O(1).
    pub(super) fn drop_reused_silent_marker(store: &mut CapStore, token: &RegexToken) {
        let RegexAtom::Named(atom_name) = &token.atom else {
            return;
        };
        let marker_sym = atom_name.spec().silent_marker_sym;
        let remove_marker = store
            .caps_mut()
            .named
            .get_mut(&marker_sym)
            .is_some_and(|slot| {
                slot.nodes.pop();
                slot.nodes.is_empty()
            });
        if remove_marker {
            store.caps_mut().named.remove(&marker_sym);
        }
    }
}
