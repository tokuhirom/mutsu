//! Settling numbered captures into the positional axis (#10895).
//!
//! A capture level that holds a numbered alias (`$N=`) is numbered statically
//! at parse time (`runtime::regex_parse_numbering`): each of its positional
//! captures is filed on the named axis under its slot number, spelled in
//! digits, so that a slot filled twice or under a quantifier accumulates
//! exactly as a repeated name does. When the level finishes, those entries
//! move to the positional slot they name; slots no capture filled stay unset
//! (`Nil`).

use super::{NamedCaptureMap, NamedSlot, PosSlot, RegexCaptures};
use crate::symbol::Symbol;

/// The positional slot a named-axis key stands for: a key spelled only in
/// decimal digits, which no user name can be (a capture name starts with a
/// letter or `_`).
// Cost: O(k), k = the key's length.
fn numbered_key_index(key: &Symbol) -> Option<usize> {
    let name = key.as_str();
    if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    name.parse().ok()
}

/// Does `named` hold captures still waiting to be settled?
// Cost: O(n), n = the distinct names.
pub(crate) fn has_numbered_captures(named: &NamedCaptureMap) -> bool {
    named.keys().any(|key| numbered_key_index(key).is_some())
}

/// The positional slot holding a named slot's entries: one Match, or a list
/// when the number was filled more than once or under a quantifier.
// Cost: O(e), e = the slot's entries.
fn pos_slot_of(slot: NamedSlot) -> PosSlot {
    if slot.nodes.len() == 1 && !slot.quantified {
        let node = slot.nodes[0].clone();
        return PosSlot {
            from: node.from,
            to: node.to,
            subcap: Some(node),
            ..Default::default()
        };
    }
    PosSlot::folded(
        slot.nodes
            .iter()
            .map(|node| (node.from, node.to, Some(node.clone())))
            .collect(),
    )
}

/// Move the digit-named entries of `named` into `positional`, at the slots
/// they name. A level that has none (every level without a numbered alias) is
/// left untouched after one scan of its names.
// Cost: O(n + p), n = the distinct names, p = the positional slots.
pub(crate) fn settle_numbered_captures(positional: &mut Vec<PosSlot>, named: &mut NamedCaptureMap) {
    if !has_numbered_captures(named) {
        return;
    }
    let numbered: Vec<(usize, NamedSlot)> = named
        .take_where(|key| numbered_key_index(key).is_some())
        .into_iter()
        .filter_map(|(key, slot)| numbered_key_index(&key).map(|idx| (idx, slot)))
        .collect();
    for (idx, slot) in numbered {
        if positional.len() <= idx {
            positional.resize_with(idx + 1, || PosSlot {
                nil: true,
                ..Default::default()
            });
        }
        positional[idx] = pos_slot_of(slot);
    }
}

impl RegexCaptures {
    /// [`settle_numbered_captures`] on this accumulator's own axes.
    // Cost: O(n + p), n = the distinct names, p = the positional slots.
    pub(crate) fn settle_numbered_captures(&mut self) {
        settle_numbered_captures(&mut self.positional, &mut self.named);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::regex_caps::CapNode;
    use std::sync::Arc;

    fn node(from: usize) -> Arc<CapNode> {
        Arc::new(CapNode {
            from,
            to: from + 1,
            ..Default::default()
        })
    }

    #[test]
    fn numbered_names_move_to_their_slots() {
        let mut named = NamedCaptureMap::default();
        named.slot_mut(Symbol::intern("x")).nodes.push(node(9));
        named.slot_mut(Symbol::intern("3")).nodes.push(node(1));
        named.slot_mut(Symbol::intern("3")).nodes.push(node(2));
        named.slot_mut(Symbol::intern("0")).nodes.push(node(0));
        let mut positional = Vec::new();
        settle_numbered_captures(&mut positional, &mut named);
        assert_eq!(named.keys().map(|k| k.resolve()).collect::<Vec<_>>(), ["x"]);
        assert_eq!(positional.len(), 4);
        assert!(positional[0].quantified.is_none() && positional[0].from == 0);
        assert!(positional[1].nil && positional[2].nil);
        assert_eq!(positional[3].quantified.as_ref().map(Vec::len), Some(2));
    }

    #[test]
    fn a_level_without_numbers_is_untouched() {
        let mut named = NamedCaptureMap::default();
        named.slot_mut(Symbol::intern("a")).nodes.push(node(0));
        let mut positional = vec![PosSlot::span(0, 1)];
        settle_numbered_captures(&mut positional, &mut named);
        assert_eq!(named.len(), 1);
        assert_eq!(positional.len(), 1);
    }
}
