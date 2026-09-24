//! Span selection for the grammar reduce-action replay
//! (`replay_reduce_action_entries`).

use crate::runtime::CapNode;

/// Indices, in ascending order, of the entries whose span is not contained in
/// another entry's span. Among entries with an equal span only the LAST logged
/// one survives: the matcher recurses, so children are logged before their
/// parent, and the outer rule is the one to dispatch (`token a { <b> }`).
///
/// This used to test every pair, which was quadratic in the reduce log; once
/// childless silent calls with an action (`<.space>` with `method space`) were
/// logged too, a 60-row YAMLish parse spent 57% of its instructions here
/// ([#9286](https://github.com/tokuhirom/mutsu/issues/9286)).
// Cost: O(n log n), n = entries.len() (one sort, one linear sweep).
pub(super) fn maximal_span_indices<T>(entries: &[(T, std::sync::Arc<CapNode>)]) -> Vec<usize> {
    let span = |i: usize| (entries[i].1.from, entries[i].1.to);
    // Start ascending, end descending: every span that can contain another
    // is ordered before it. Equal spans put the last-logged one first so it
    // is the one kept.
    let mut order: Vec<usize> = (0..entries.len()).collect();
    order.sort_unstable_by(|&a, &b| {
        let (af, at) = span(a);
        let (bf, bt) = span(b);
        af.cmp(&bf).then(bt.cmp(&at)).then(b.cmp(&a))
    });
    let mut kept = Vec::new();
    // The furthest end among spans already swept. Each of them starts at or
    // before the current span, so it contains the current one exactly when
    // it also ends at or after it.
    let mut max_to: Option<usize> = None;
    for i in order {
        let (_, to) = span(i);
        if max_to.is_some_and(|m| m >= to) {
            continue;
        }
        kept.push(i);
        max_to = Some(to);
    }
    kept.sort_unstable();
    kept
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn node(from: usize, to: usize) -> ((), Arc<CapNode>) {
        (
            (),
            Arc::new(CapNode {
                from,
                to,
                ..Default::default()
            }),
        )
    }

    /// The pairwise definition this replaces, kept as the oracle.
    fn quadratic(entries: &[((), Arc<CapNode>)]) -> Vec<usize> {
        (0..entries.len())
            .filter(|&i| {
                let e = &entries[i].1;
                !entries.iter().enumerate().any(|(j, (_, f))| {
                    j != i
                        && f.from <= e.from
                        && e.to <= f.to
                        && ((f.from, f.to) != (e.from, e.to) || j > i)
                })
            })
            .collect()
    }

    #[test]
    fn keeps_outermost_and_last_equal_span() {
        let entries = vec![node(0, 2), node(0, 5), node(0, 5), node(6, 6), node(5, 7)];
        assert_eq!(maximal_span_indices(&entries), vec![2, 4]);
    }

    #[test]
    fn matches_pairwise_definition() {
        let mut seed = 0x2545_f491_u64;
        for len in 0..40 {
            let entries: Vec<_> = (0..len)
                .map(|_| {
                    seed ^= seed << 13;
                    seed ^= seed >> 7;
                    seed ^= seed << 17;
                    let from = (seed % 8) as usize;
                    let to = from + ((seed >> 8) % 4) as usize;
                    node(from, to)
                })
                .collect();
            assert_eq!(maximal_span_indices(&entries), quadratic(&entries));
        }
    }
}
