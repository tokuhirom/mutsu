//! Tiers for the candidates of a dispatch that are actually read.
//!
//! [`assign_tiers_with`] compares every pair of the `n` candidates of a
//! dispatch and peels the relation layer by layer. A method dispatch reads the
//! tier of the candidates that bound the call only (plus whether a tier holds
//! a candidate that needs a trial bind), and a candidate's tier is the length
//! of the longest chain of "narrower" candidates above it. That is a function
//! of the candidate's ancestors in the relation, a handful of types, not of
//! the other `n - 2` candidates, so [`assign_tiers_wanted`] walks only those.
//! `RakuAST::Deparse` has 267 candidates and a call binds two or three of them.
//!
//! The walk is exact where the relation is acyclic above every candidate it
//! visits; a cycle (the relation can form one across an arity mismatch) makes
//! the layer peeling close the remaining candidates into one tier, which
//! depends on every candidate, so the walk then gives up and the whole
//! relation is built as before.

use super::multi_rank::{MultiRank, assign_tiers_with};
use crate::symbol::Symbol;

/// [`assign_tiers_with`] for a caller that reads the tiers of `items[..wanted]`
/// only. The tier of an item at or beyond `wanted` is left untouched unless the
/// walk visited it, and must not be read.
// Cost: O(n * k) where k = the candidates the walk visits (the wanted ones, the
// ones that need a trial bind and their ancestors in the relation); O(n^2 * p)
// as `assign_tiers_with` when the walk meets a cycle or `wanted >= n`.
pub(crate) fn assign_tiers_wanted<T>(
    items: &mut [T],
    wanted: usize,
    rank: impl Fn(&mut T) -> &mut MultiRank,
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
    declared_pair: &mut dyn FnMut(usize, usize, &MultiRank, &MultiRank) -> bool,
) {
    let n = items.len();
    if wanted >= n || n < 2 {
        assign_tiers_with(items, rank, conforms, declared_pair);
        return;
    }
    let ranks: Vec<MultiRank> = items.iter_mut().map(|item| *rank(item)).collect();
    let Some(depth) = walk_depths(&ranks, wanted, conforms, declared_pair) else {
        assign_tiers_with(items, rank, conforms, declared_pair);
        return;
    };
    let mut tier_has_bind_check: Vec<bool> = vec![false; depth.iter().flatten().max().map_or(1, |m| *m as usize + 1)];
    for (r, d) in ranks.iter().zip(&depth) {
        if needs_bind_check(r)
            && let Some(d) = d
        {
            tier_has_bind_check[*d as usize] = true;
        }
    }
    for (item, d) in items.iter_mut().zip(&depth) {
        let r = rank(item);
        if let Some(d) = d {
            r.tier = *d;
            r.tiered = true;
            r.tier_bind_check = tier_has_bind_check[*d as usize];
        }
    }
}

/// A candidate whose parameter types can take the arguments at all and that
/// carries a literal or a refinement: it is in the trial-bind pool.
// Cost: O(1).
fn needs_bind_check(r: &MultiRank) -> bool {
    r.arity_fits && !r.profile.unrelated && (r.nominal.0 > 0 || r.refinement[0] > 0)
}

/// Whether candidate `i` is narrower than candidate `j` in the relation
/// `assign_tiers_with` builds.
// Cost: O(1) plus one `conforms`/`declared_pair` call.
fn narrower(
    ranks: &[MultiRank],
    i: usize,
    j: usize,
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
    declared_pair: &mut dyn FnMut(usize, usize, &MultiRank, &MultiRank) -> bool,
) -> bool {
    if i == j {
        return false;
    }
    if ranks[i].profile.unrelated || ranks[j].profile.unrelated {
        return ranks[i].invocant.is_some()
            && ranks[j].invocant.is_some()
            && declared_pair(i, j, &ranks[i], &ranks[j]);
    }
    ranks[i].rakudo_narrower(&ranks[j], conforms)
}

/// The tier (chain depth) of `items[..wanted]` and of every candidate in the
/// trial-bind pool, `None` entries for the others. `None` overall when a cycle
/// shows up above a visited candidate.
// Cost: O(n * k), k = candidates visited.
fn walk_depths(
    ranks: &[MultiRank],
    wanted: usize,
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
    declared_pair: &mut dyn FnMut(usize, usize, &MultiRank, &MultiRank) -> bool,
) -> Option<Vec<Option<u32>>> {
    let n = ranks.len();
    // 0 = not seen, 1 = on the walk's stack, 2 = depth known.
    let mut state = vec![0u8; n];
    let mut depth: Vec<Option<u32>> = vec![None; n];
    let mut preds: Vec<Option<Vec<usize>>> = vec![None; n];
    let targets = (0..n).filter(|&i| i < wanted || needs_bind_check(&ranks[i]));
    for root in targets {
        if state[root] == 2 {
            continue;
        }
        // Explicit stack: the chain above a candidate is as long as the
        // relation makes it, not as long as anything bounded.
        let mut stack: Vec<usize> = vec![root];
        state[root] = 1;
        while let Some(&j) = stack.last() {
            let row = preds[j].get_or_insert_with(|| {
                (0..n)
                    .filter(|&i| narrower(ranks, i, j, conforms, declared_pair))
                    .collect()
            });
            let mut pending = None;
            let mut best = 0u32;
            for &i in row.iter() {
                match state[i] {
                    2 => best = best.max(depth[i].map_or(0, |d| d + 1)),
                    1 => return None,
                    _ => {
                        pending = Some(i);
                        break;
                    }
                }
            }
            if let Some(i) = pending {
                state[i] = 1;
                stack.push(i);
            } else {
                depth[j] = Some(best);
                state[j] = 2;
                stack.pop();
            }
        }
    }
    Some(depth)
}
