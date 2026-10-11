//! The one narrowness relation multi dispatch ranks candidates by.
//!
//! A multi sub and a multi method are both ranked by a [`MultiRank`]: a
//! lexicographic *tier* order (nominal types, type distance, refinements,
//! slurpiness, named bind check, optional positionals) refined by rakudo's
//! parameter-wise partial order, which [`RankProfile`] answers. The sub
//! resolver, the cached dispatch plan, the per-type dispatch program and the
//! method resolver all compare, scan and prune through this one type, so the
//! rules cannot drift between them (#12468).

use crate::symbol::Symbol;
use std::cmp::Ordering;

/// How many leading positional parameters a [`RankProfile`] records.
pub(crate) const PROFILE_LEN: usize = 6;

/// A candidate's per-positional-parameter narrowness, for the one question the
/// scalar tier order cannot answer: are two candidates *comparable*?
///
/// Rakudo orders candidates parameter-wise: one is narrower only when it is at
/// least as narrow on every positional parameter and strictly narrower on one,
/// where a `where`/`subset`/literal refinement counts as narrower than the same
/// type without it. When each candidate wins a different parameter (one by a
/// refinement, the other by a nominal type) they are incomparable, and a
/// refinement means a bind-time check, so the first candidate DECLARED that
/// binds wins instead of the summed key picking one
/// ([#11943](https://github.com/tokuhirom/mutsu/issues/11943)).
///
/// `scores[i]` is `2 * distance + (1 if unrefined)`, lower being narrower, and
/// `u16::MAX` for a parameter the candidate does not have.
#[derive(Clone, Copy, Debug)]
pub(crate) struct RankProfile {
    scores: [u16; PROFILE_LEN],
    /// Each parameter's nominal type (`None`: not recorded), so a score
    /// difference is only read as "narrower" between related types.
    nominal: [Option<Symbol>; PROFILE_LEN],
    refined: bool,
}

impl RankProfile {
    pub(crate) const EMPTY: RankProfile = RankProfile {
        scores: [u16::MAX; PROFILE_LEN],
        nominal: [None; PROFILE_LEN],
        refined: false,
    };

    /// Record positional parameter `slot`: its type-hierarchy distance from the
    /// argument, whether it carries a refinement (`where`, literal, subset) and
    /// its nominal type. Shared by the sub and method rankers so both feed the
    /// one [`Self::incomparable`] relation.
    // Cost: O(1).
    pub(crate) fn record(
        &mut self,
        slot: usize,
        delta: usize,
        refined: bool,
        nominal: Option<Symbol>,
    ) {
        if slot < PROFILE_LEN {
            let d = delta.min(20_000) as u16;
            self.scores[slot] = d * 2 + u16::from(!refined);
            self.nominal[slot] = nominal;
            self.refined |= refined;
        }
    }

    /// Whether neither candidate is narrower than the other because of a
    /// refinement: each wins at least one shared parameter, and at least one of
    /// the two carries a refinement (a purely nominal split stays the
    /// ambiguity the tier order reports).
    // Cost: O(1), the profile length is a constant.
    pub(crate) fn incomparable(
        &self,
        other: &RankProfile,
        related: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        if !(self.refined || other.refined) {
            return false;
        }
        let (mut self_wins, mut other_wins) = (false, false);
        for i in 0..PROFILE_LEN {
            let (a, b) = (self.scores[i], other.scores[i]);
            if a == u16::MAX || b == u16::MAX {
                continue;
            }
            // Unrelated nominal types (`Node` refined by a subset vs an
            // unrelated role) are tied in rakudo's per-parameter comparison;
            // their distances and the refinement bit say nothing.
            if let (Some(na), Some(nb)) = (self.nominal[i], other.nominal[i])
                && na != nb
                && !related(na, nb)
            {
                continue;
            }
            self_wins |= a < b;
            other_wins |= b < a;
        }
        self_wins && other_wins
    }
}

/// The narrowness of one multi candidate against one call's arguments.
///
/// The tier order ([`Self::tier_cmp`]) compares, narrowest first:
///
/// 0. the NOMINAL tier — literal-value count, then meaningfully-typed
///    positional count (higher is narrower; a method ranks literals only);
/// 1. the type-hierarchy distance of those nominal types (lower is narrower);
/// 2. the REFINEMENT tier, compared lexicographically, higher is narrower. A
///    sub fills it with (constrained-parameter count — a `where` clause or a
///    `subset` type, rakudo draws no line between the two —, sub-signature
///    count, `rw`/`raw` count, 0); a method with (literals at shared
///    positions, constrained count, sigilled positional count, `rw`/`raw`
///    count);
/// 3. whether it declares no slurpy positional (rakudo's `is_narrower`
///    consults slurpiness only once every shared positional type is tied, and
///    a non-slurpy candidate wins there), then whether it needs a named bind
///    check (an explicit named parameter, or a `where` on a slurpy);
/// 4. optional-positional count (fewer is narrower).
///
/// [`Self::total_cmp`] then breaks a tier tie by declaration order.
///
/// The tier split is rakudo's rule, not a mutsu invention: its `is_narrower`
/// compares the candidates' nominal parameter types first and only consults a
/// parameter's constraint (`where`, and the equality check a literal or a
/// `subset` compiles to) when the two candidates are TIED on every nominal
/// type. So `multi f($x where ...)` must lose to `multi f(Int $x)` for
/// `f(42)`, while `multi f(Int $x where ...)` still beats `multi f(Int $x)`
/// ([#8958](https://github.com/tokuhirom/mutsu/issues/8958)).
#[derive(Clone, Copy, Debug)]
pub(crate) struct MultiRank {
    pub(crate) nominal: (usize, usize),
    pub(crate) distance: usize,
    pub(crate) refinement: [usize; 4],
    pub(crate) non_slurpy: bool,
    pub(crate) bind_check: bool,
    pub(crate) optional: usize,
    /// Declaration order: a sub's `decl_order` stamp, a method's index in
    /// its MRO-ordered match list.
    pub(crate) decl_order: u64,
    pub(crate) profile: RankProfile,
}

impl MultiRank {
    /// The tier order, narrowest first, ignoring declaration order: two
    /// candidates comparing `Equal` here are tied — they join the
    /// `X::Multi::Ambiguous` set, and a narrowest-first scan may not stop
    /// between them.
    // Cost: O(1).
    pub(crate) fn tier_cmp(&self, other: &MultiRank) -> Ordering {
        other
            .nominal
            .cmp(&self.nominal)
            .then(self.distance.cmp(&other.distance))
            .then(other.refinement.cmp(&self.refinement))
            .then((other.non_slurpy, other.bind_check).cmp(&(self.non_slurpy, self.bind_check)))
            .then(self.optional.cmp(&other.optional))
    }

    /// [`Self::tier_cmp`], then declaration order: the total order a winner is
    /// picked by — of tied candidates, the one declared first, which is what
    /// Rakudo runs.
    // Cost: O(1).
    pub(crate) fn total_cmp(&self, other: &MultiRank) -> Ordering {
        self.tier_cmp(other)
            .then(self.decl_order.cmp(&other.decl_order))
    }

    /// Whether `self` and `other` are incomparable in rakudo's parameter-wise
    /// narrowness order (see [`RankProfile`]).
    // Cost: O(r), r = the cost of one `related` call.
    pub(crate) fn incomparable(
        &self,
        other: &MultiRank,
        related: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        self.profile.incomparable(&other.profile, related)
    }

    /// Whether `self` is strictly narrower than `other`: comparable with it
    /// and ahead of it in the tier order.
    // Cost: O(r), r = the cost of one `related` call.
    pub(crate) fn is_narrower(
        &self,
        other: &MultiRank,
        related: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        self.tier_cmp(other) == Ordering::Less && !self.incomparable(other, related)
    }

    /// Whether a narrowest-first scan that already bound a candidate ranked
    /// `best` may skip `self` without trying it: `self` is strictly wider and
    /// comparable with `best`. One that is incomparable with it may still win
    /// by declaration order (#11943), so it is tried.
    // Cost: O(r), r = the cost of one `related` call.
    pub(crate) fn scan_may_skip(
        &self,
        best: &MultiRank,
        related: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        best.is_narrower(self, related)
    }
}

/// Drop the matches another match is narrower than and, when what is left is
/// split by incomparable candidates, keep only the first declared of them. A
/// pure tie (every survivor tier-equal) is returned whole, in the input
/// order, so the caller's ambiguity logic still sees it.
// Cost: O(m^2 * r), m = matches, r = the cost of one `related` call.
pub(crate) fn prune_to_maximal<T>(
    matches: Vec<(MultiRank, T)>,
    related: &dyn Fn(Symbol, Symbol) -> bool,
) -> Vec<(MultiRank, T)> {
    if matches.len() < 2 {
        return matches;
    }
    let ranks: Vec<MultiRank> = matches.iter().map(|(r, _)| *r).collect();
    let dominated = |k: &MultiRank| ranks.iter().any(|o| o.is_narrower(k, related));
    let mut maximal: Vec<(MultiRank, T)> =
        matches.into_iter().filter(|(k, _)| !dominated(k)).collect();
    let first = maximal[0].0;
    if maximal
        .iter()
        .all(|(k, _)| k.tier_cmp(&first) == Ordering::Equal)
    {
        return maximal;
    }
    // Incomparable survivors: declaration order decides.
    let keep = maximal
        .iter()
        .enumerate()
        .min_by_key(|(_, (k, _))| k.decl_order)
        .map(|(i, _)| i)
        .unwrap_or(0);
    vec![maximal.swap_remove(keep)]
}
