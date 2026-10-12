//! Whether two stored capture nodes present the same `Match` (#12918).

use super::{CapChildren, CapNode, PosSlot};
use std::sync::Arc;

impl CapNode {
    /// Whether a `Match` over `other` reads exactly as one over `self`: the
    /// same span and dispatch metadata, and the very same child nodes. Lets a
    /// caller that rebuilt a node from the captures another `Match` already
    /// presents adopt that `Match` for it (a wrapped token's `callsame`
    /// result filed as the caller's `$<rule>`, as rakudo files it).
    ///
    /// Children compare by `Arc` identity, never by content; a node carrying
    /// reduce-time state (`ast`, `:my $*x` values, a cursor) is never alike.
    // Cost: O(c), c = the child slots of the two nodes.
    pub(crate) fn presents_like(&self, other: &CapNode) -> bool {
        self.from == other.from
            && self.to == other.to
            && self.sym == other.sym
            && self.action_name == other.action_name
            && self.ast.is_none()
            && other.ast.is_none()
            && match (self.children.as_deref(), other.children.as_deref()) {
                (None, None) => true,
                (Some(a), Some(b)) => children_alike(a, b),
                _ => false,
            }
    }
}

fn children_alike(a: &CapChildren, b: &CapChildren) -> bool {
    a.pos == b.pos
        && a.regex_vars.is_empty()
        && b.regex_vars.is_empty()
        && a.cursor.is_none()
        && b.cursor.is_none()
        && a.capture_alias_map == b.capture_alias_map
        && a.positional.len() == b.positional.len()
        && a.positional
            .iter()
            .zip(&b.positional)
            .all(|(x, y)| pos_slots_alike(x, y))
        && a.named.len() == b.named.len()
        && a.named
            .iter()
            .zip(b.named.iter())
            .all(|((ka, sa), (kb, sb))| {
                ka == kb && sa.quantified == sb.quantified && same_nodes(&sa.nodes, &sb.nodes)
            })
}

fn pos_slots_alike(a: &PosSlot, b: &PosSlot) -> bool {
    a.from == b.from
        && a.to == b.to
        && a.nil == b.nil
        && a.alternation_padding == b.alternation_padding
        && same_node(&a.subcap, &b.subcap)
        && match (&a.quantified, &b.quantified) {
            (None, None) => true,
            (Some(qa), Some(qb)) => {
                qa.len() == qb.len()
                    && qa
                        .iter()
                        .zip(qb)
                        .all(|(x, y)| x.0 == y.0 && x.1 == y.1 && same_node(&x.2, &y.2))
            }
            _ => false,
        }
}

fn same_node(a: &Option<Arc<CapNode>>, b: &Option<Arc<CapNode>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        _ => false,
    }
}

fn same_nodes(a: &[Arc<CapNode>], b: &[Arc<CapNode>]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| Arc::ptr_eq(x, y))
}
