//! The one narrowness relation multi dispatch ranks candidates by.
//!
//! A multi sub and a multi method are both ranked by a [`MultiRank`]: a
//! lexicographic *tier* order (nominal types, type distance, refinements,
//! slurpiness, named bind check, optional positionals) refined by rakudo's
//! parameter-wise partial order, which [`RankProfile`] answers. The sub
//! resolver, the cached dispatch plan, the per-type dispatch program and the
//! method resolver all compare, scan and prune through this one type, so the
//! rules cannot drift between them (#12468).

use crate::ast::ParamDef;
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
    /// difference is only read as "narrower" between related types. For an
    /// unreached slot (see `unreached`) it is the declared type.
    nominal: [Option<Symbol>; PROFILE_LEN],
    /// Each parameter's DECLARED type (`None`: a literal or a slurpy, whose
    /// type is not compared), independent of the argument. Rakudo's tiers
    /// are built from declared types, so [`Self::declared_wins`] compares a
    /// pair by these even when an argument is unrelated to one of them.
    declared: [Option<Symbol>; PROFILE_LEN],
    /// Bit `i` is set when positional slot `i` has a coercion type. Rakudo
    /// expands coercions into target and accepted-type candidates, affecting
    /// narrowness across arity mismatches.
    coercive: u8,
    /// Bit `i` set: slot `i` is an optional positional the call supplies no
    /// argument for. Its score says nothing (there is no argument to measure
    /// a distance from), so such a slot is compared by its declared type in
    /// [`Self::unreached_wins`] instead.
    unreached: u8,
    refined: bool,
    /// Some positional argument is unrelated to its parameter's type: the
    /// candidate cannot bind these arguments, and its distances say nothing
    /// about its narrowness (see [`assign_tiers`]).
    unrelated: bool,
    /// Some positional argument carries a mixed-in role that its parameter
    /// names: that parameter scores nearer than the value's own type would,
    /// though the role and another candidate's nominal type may be unrelated
    /// (see [`MultiRank::narrower_by_rank`]).
    role_mixin: bool,
}

impl RankProfile {
    pub(crate) const EMPTY: RankProfile = RankProfile {
        scores: [u16::MAX; PROFILE_LEN],
        nominal: [None; PROFILE_LEN],
        declared: [None; PROFILE_LEN],
        coercive: 0,
        unreached: 0,
        refined: false,
        unrelated: false,
        role_mixin: false,
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
        declared: Option<Symbol>,
        coercive: bool,
    ) {
        if slot < PROFILE_LEN {
            let d = delta.min(20_000) as u16;
            self.scores[slot] = d * 2 + u16::from(!refined);
            self.nominal[slot] = nominal;
            self.declared[slot] = declared;
            if coercive {
                self.coercive |= 1 << slot;
            }
            self.refined |= refined;
        }
    }

    /// Mark slot `slot` as an optional positional the call does not reach,
    /// declared with the nominal type `declared` (see [`Self::unreached_wins`]).
    /// Called after the slot's [`Self::record`].
    // Cost: O(1).
    pub(crate) fn mark_unreached(&mut self, slot: usize, declared: Symbol) {
        if slot < PROFILE_LEN {
            self.unreached |= 1 << slot;
            self.nominal[slot] = Some(declared);
        }
    }

    /// How many of the first `n` positional slots carry a refinement.
    // Cost: O(1) (at most `PROFILE_LEN` slots).
    fn refined_below(&self, n: usize) -> usize {
        self.scores
            .iter()
            .take(n)
            .filter(|&&score| score != u16::MAX && score % 2 == 0)
            .count()
    }

    /// Record that some positional argument is unrelated to its parameter's
    /// type (the type-distance walk scored the "unrelated" distance).
    // Cost: O(1).
    pub(crate) fn mark_unrelated(&mut self) {
        self.unrelated = true;
    }

    /// Record that a positional argument's mixed-in role satisfies its
    /// parameter's type.
    // Cost: O(1).
    pub(crate) fn mark_role_mixin(&mut self) {
        self.role_mixin = true;
    }

    /// Whether a coercion parameter appears in the positional prefix shared
    /// by these two candidates.
    // Cost: O(1).
    fn shares_coercion_with(&self, other: &RankProfile, shared: usize) -> bool {
        let mask = (1u8 << shared.min(PROFILE_LEN)) - 1;
        (self.coercive | other.coercive) & mask != 0
    }

    /// Whether neither candidate is narrower than the other because of a
    /// refinement: each wins at least one shared parameter, and at least one of
    /// the two carries a refinement (a purely nominal split stays the
    /// ambiguity the tier order reports).
    // Cost: O(1), the profile length is a constant.
    pub(crate) fn incomparable(
        &self,
        other: &RankProfile,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        if !(self.refined || other.refined) {
            return false;
        }
        let (mut self_wins, mut other_wins) = (false, false);
        for i in 0..PROFILE_LEN {
            let (a, b) = (self.scores[i], other.scores[i]);
            if a == u16::MAX || b == u16::MAX || (self.unreached | other.unreached) & (1 << i) != 0 {
                continue;
            }
            // Unrelated nominal types (`Node` refined by a subset vs an
            // unrelated role) are tied in rakudo's per-parameter comparison;
            // their distances and the refinement bit say nothing.
            if let (Some(na), Some(nb)) = (self.nominal[i], other.nominal[i])
                && na != nb
                && !conforms(na, nb)
                && !conforms(nb, na)
            {
                continue;
            }
            self_wins |= a < b;
            other_wins |= b < a;
        }
        self_wins && other_wins
    }

    /// Whether some positional slot both candidates type has a different
    /// DECLARED type in each. With equal distances those types are unrelated
    /// (a related pair differs in distance), and rakudo then ties the pair
    /// instead of reading `is rw` / `is raw` as narrowness.
    // Cost: O(1), the profile length is a constant.
    pub(crate) fn declared_differs(&self, other: &RankProfile) -> bool {
        (0..PROFILE_LEN).any(|i| {
            matches!((self.declared[i], other.declared[i]), (Some(a), Some(b)) if a != b)
        })
    }

    /// Rakudo's per-parameter comparison by DECLARED type over the positional
    /// slots both candidates type: `(self wins one, other wins one)`. A type
    /// that strictly conforms to the other's wins; the same type is won by a
    /// refinement (`where`, literal, subset) and unrelated types are skipped
    /// (neither narrower nor wider).
    // Cost: O(r), r = the cost of one `conforms` call; the profile length is a constant.
    pub(crate) fn declared_wins(
        &self,
        other: &RankProfile,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> (bool, bool) {
        let (mut self_wins, mut other_wins) = (false, false);
        for i in 0..PROFILE_LEN {
            let (Some(a), Some(b)) = (self.declared[i], other.declared[i]) else {
                continue;
            };
            if a == b {
                // The low score bit is "unrefined".
                let (ra, rb) = (self.scores[i] & 1, other.scores[i] & 1);
                self_wins |= ra < rb;
                other_wins |= rb < ra;
                continue;
            }
            match (conforms(a, b), conforms(b, a)) {
                (true, false) => self_wins = true,
                (false, true) => other_wins = true,
                _ => {}
            }
        }
        (self_wins, other_wins)
    }

    /// Rakudo's per-parameter comparison over the optional positionals the
    /// call reaches in neither candidate: `(self wins one, other wins one)`.
    /// Rakudo's tiers are built from the DECLARED types, so such a parameter
    /// still orders the pair: `multi g(Int $a?)` is narrower than
    /// `multi g($a?)` for `g()`. A type that conforms to the other's wins; the
    /// same type wins by a refinement; unrelated types (`Int` / `Str`) tie,
    /// which leaves the two candidates in one tier — `X::Multi::Ambiguous`
    /// when both bind (#12852).
    // Cost: O(r), r = the cost of one `conforms` call; the profile length is a constant.
    pub(crate) fn unreached_wins(
        &self,
        other: &RankProfile,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> (bool, bool) {
        let shared = self.unreached & other.unreached;
        let (mut self_wins, mut other_wins) = (false, false);
        if shared == 0 {
            return (false, false);
        }
        for i in 0..PROFILE_LEN {
            if shared & (1 << i) == 0 {
                continue;
            }
            let (Some(na), Some(nb)) = (self.nominal[i], other.nominal[i]) else {
                continue;
            };
            if na == nb {
                // The low score bit is "unrefined".
                self_wins |= self.scores[i] < other.scores[i];
                other_wins |= other.scores[i] < self.scores[i];
            } else if conforms(na, nb) {
                self_wins = true;
            } else if conforms(nb, na) {
                other_wins = true;
            }
        }
        (self_wins, other_wins)
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
/// 4. whether it declares an optional positional (one that does not is
///    narrower).
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
    /// Which component of [`Self::refinement`] counts the constrained
    /// parameters (`where`, `subset`): 0 for a sub, 1 for a method (whose
    /// component 0 counts literals).
    pub(crate) constrained_slot: usize,
    pub(crate) non_slurpy: bool,
    /// `(positional count, required positional count)`, slurpies excluded:
    /// rakudo's `num_types` and `min_arity`, for the arity-mismatch rule of
    /// [`Self::is_narrower`].
    pub(crate) arity: (usize, usize),
    pub(crate) bind_check: bool,
    /// Whether the candidate declares an optional positional
    /// ([`has_optional_positional`]).
    pub(crate) optional: bool,
    /// Declaration order: a sub's `decl_order` stamp, a method's index in
    /// its MRO-ordered match list.
    pub(crate) decl_order: u64,
    pub(crate) profile: RankProfile,
    /// The class declaring a multi METHOD candidate (`None` for a sub). Rakudo
    /// types a method candidate's invocant by its declaring class and compares
    /// it as the first parameter, so a subclass's candidate is narrower than
    /// an ancestor's on it ([#13507](https://github.com/tokuhirom/mutsu/issues/13507)).
    pub(crate) invocant: Option<Symbol>,
    /// Rakudo's tier: the candidate's layer in the topological sort of the
    /// whole candidate set by narrowness, the first (narrowest) being 0 (see
    /// [`assign_tiers`]). It leads every comparison, so a candidate in a
    /// later tier never wins over a binding one in an earlier tier, even
    /// when the pair alone is incomparable. 0 everywhere when the caller
    /// ranks only candidates that already bound (multi methods).
    pub(crate) tier: u32,
    /// Whether [`assign_tiers`] ranked this candidate against the whole
    /// candidate set. Only then is an arity-incomparable pair left
    /// incomparable within a tier (see [`Self::is_narrower`]).
    pub(crate) tiered: bool,
    /// Whether some candidate in this one's tier (matching the call or not)
    /// carries a literal or a `where`/`subset` refinement. Rakudo resolves a
    /// tier that holds such a candidate by trial binding, first declared
    /// first, so a tie inside it is never `X::Multi::Ambiguous`. Set by
    /// [`assign_tiers`].
    pub(crate) tier_bind_check: bool,
    /// Whether the call's positional argument count fits the candidate's
    /// arity; a candidate that cannot take the call is not in the pool a tier
    /// is trial-bound from ([`Self::tier_bind_check`]).
    pub(crate) arity_fits: bool,
}

impl MultiRank {
    /// The tier order, narrowest first, ignoring declaration order: two
    /// candidates comparing `Equal` here are tied — they join the
    /// `X::Multi::Ambiguous` set, and a narrowest-first scan may not stop
    /// between them.
    // Cost: O(1).
    pub(crate) fn tier_cmp(&self, other: &MultiRank) -> Ordering {
        self.tier.cmp(&other.tier).then(self.rank_cmp(other))
    }

    /// [`Self::tier_cmp`] without the topological tier: the per-candidate
    /// lexicographic order alone.
    // Cost: O(1).
    fn rank_cmp(&self, other: &MultiRank) -> Ordering {
        other
            .nominal
            .cmp(&self.nominal)
            .then(self.distance.cmp(&other.distance))
            .then(self.refinement_cmp(other))
            .then((other.non_slurpy, other.bind_check).cmp(&(self.non_slurpy, self.bind_check)))
            .then(self.optional.cmp(&other.optional))
    }

    /// The refinement tier of [`Self::rank_cmp`]: the candidate with more
    /// refined parameters (`where`, `subset`) is narrower, except that a
    /// refinement on a positional the other candidate does not declare is
    /// no evidence against a candidate that carries a bind check of its own.
    /// Rakudo compares the constraints of the positionals two candidates
    /// share, and a refined candidate and a named-parameter one both carry
    /// its bind-check flag: `multi f(UInt $s = 1, :$a)` ties `multi f(:$a)`
    /// for `f()` (declaration order decides), while `multi f(UInt $s = 1)`
    /// still beats a bare `multi f()`.
    // Cost: O(1).
    fn refinement_cmp(&self, other: &MultiRank) -> Ordering {
        // `is rw` / `is raw` only breaks a tie between parameters of the same
        // type (`multi f(TH $h)` / `multi f(Hash $h is raw)` tie for a value
        // that is both).
        let writable = if self.constrained_slot == 0 { 2 } else { 3 };
        let by_type = self.profile.declared_differs(&other.profile);
        let key = |r: &MultiRank| {
            let mut key = r.refinement;
            if by_type {
                key[writable] = 0;
            }
            key
        };
        let ord = key(other).cmp(&key(self));
        let rest = |r: &MultiRank| {
            let mut rest = key(r);
            rest[r.constrained_slot] = 0;
            rest
        };
        if ord == Ordering::Equal || rest(self) != rest(other) {
            return ord;
        }
        let less = if ord == Ordering::Less { other } else { self };
        let shared = self.arity.0.min(other.arity.0);
        if less.bind_check
            && self.profile.refined_below(shared) == other.profile.refined_below(shared)
        {
            return Ordering::Equal;
        }
        ord
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
    // Cost: O(r), r = the cost of one `conforms` call.
    pub(crate) fn incomparable(
        &self,
        other: &MultiRank,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        self.profile.incomparable(&other.profile, conforms)
    }

    /// Whether `self` is narrower than `other` with the tier and the arity
    /// rule set aside: ahead in the rank order and comparable with it, or tied
    /// there and narrower on an optional positional the call reaches in
    /// neither (see [`RankProfile::unreached_wins`]). An unreached parameter
    /// on which `other` is narrower makes the pair incomparable however the
    /// rank order reads (`multi f(Int $a, $b?)` / `multi f($a, Int $b?)` for
    /// `f(1)` is ambiguous in rakudo).
    // Cost: O(r), r = the cost of one `conforms` call.
    fn narrower_by_rank(&self, other: &MultiRank, conforms: &dyn Fn(Symbol, Symbol) -> bool) -> bool {
        // Of two method candidates declared in related classes, the subclass's
        // invocant is the narrower parameter. Rakudo then calls the pair
        // ordered only when no declared parameter type is narrower in the
        // other candidate (a `where` or a tied type decides nothing here).
        match self.invocant_order(other, conforms) {
            Some(Ordering::Greater) => return false,
            Some(Ordering::Less) => {
                return !self.profile.declared_wins(&other.profile, conforms).1;
            }
            _ => {}
        }
        let (self_wins, other_wins) = self.profile.unreached_wins(&other.profile, conforms);
        if other_wins {
            return false;
        }
        // Rakudo orders two candidates by their DECLARED types, parameter by
        // parameter. A distance artifact (a role mixed into an argument is
        // nearer than the value's own type, though the role and the other
        // candidate's nominal type are unrelated) must not put a candidate
        // ahead of one that is narrower on a declared type it does not lose
        // (#14176: `(R, Numeric)` vs `(Rational:D, Rational:D)`).
        let (decl_self, decl_other) = if self.profile.role_mixin || other.profile.role_mixin {
            self.profile.declared_wins(&other.profile, conforms)
        } else {
            (false, false)
        };
        if decl_other && !decl_self {
            return false;
        }
        match self.rank_cmp(other) {
            // Ahead in the rank order only by the argument distances is no
            // narrowness when `other` is narrower on a declared parameter and
            // `self` on none: rakudo needs `self` at least as narrow on every
            // parameter. `(ElementWrapper, Attribute, Mu)` is not narrower than
            // `(XML::Node-subset, Attribute-subset, $obj where ...)` because
            // an unrelated first parameter happened to sit nearer the argument
            // (XML::Class's `deserialise`, #15085).
            Ordering::Less => {
                !self.incomparable(other, conforms) && {
                    let (self_wins, other_wins) = self.profile.declared_wins(&other.profile, conforms);
                    self_wins || !other_wins
                }
            }
            Ordering::Equal => self_wins && !self.incomparable(other, conforms),
            Ordering::Greater => decl_self && !decl_other && self.nominal == other.nominal,
        }
    }

    /// How the invocant types of two method candidates order them: `Less`
    /// when `self`'s (a subclass of `other`'s) is narrower, `Greater` when it
    /// is wider, `None` when equal, unrelated or not a method pair.
    // Cost: O(r), r = the cost of one `conforms` call.
    fn invocant_order(
        &self,
        other: &MultiRank,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> Option<Ordering> {
        let (a, b) = (self.invocant?, other.invocant?);
        if a == b {
            return None;
        }
        match (conforms(a, b), conforms(b, a)) {
            (true, false) => Some(Ordering::Less),
            (false, true) => Some(Ordering::Greater),
            _ => None,
        }
    }

    /// [`Self::rakudo_narrower`] for a pair in which a candidate cannot bind
    /// the call's arguments (so its argument distances say nothing): compared
    /// by declared types alone, the invocant first, and tied or unrelated
    /// types leave a candidate without a slurpy positional narrower.
    // Cost: O(r), r = the cost of one `conforms` call.
    fn declared_narrower(&self, other: &MultiRank, conforms: &dyn Fn(Symbol, Symbol) -> bool) -> bool {
        if self.arity_mismatch(other) {
            return self.non_slurpy && !other.non_slurpy;
        }
        let (mut self_wins, mut other_wins) = self.profile.declared_wins(&other.profile, conforms);
        match self.invocant_order(other, conforms) {
            Some(Ordering::Less) => self_wins = true,
            Some(Ordering::Greater) => other_wins = true,
            _ => {}
        }
        if other_wins {
            return false;
        }
        self_wins || (self.non_slurpy && !other.non_slurpy)
    }

    /// Whether `self` and `other` differ in both positional count and
    /// required positional count: rakudo's arity mismatch, which normally
    /// suppresses parameter comparisons. A shared coercion parameter is the
    /// exception because Rakudo expands it into target and accepted-type
    /// candidates.
    // Cost: O(1).
    fn arity_mismatch(&self, other: &MultiRank) -> bool {
        self.arity.0 != other.arity.0 && self.arity.1 != other.arity.1
    }

    /// Rakudo's `is_narrower` on the candidates' own ranks, the relation the
    /// topological tiers of [`assign_tiers`] are built from. On an arity
    /// mismatch (see [`Self::arity_mismatch`]) a candidate without a slurpy
    /// positional is narrower than one with it, and otherwise neither is
    /// narrower unless the shared positional prefix contains a coercion type.
    /// Rakudo's coercion expansion keeps that prefix's narrowness visible:
    /// `multi m(Scalar:U, Int() $y = 0)` beats `multi m(Int() $x = 0)` for
    /// `m(Scalar)`. Without coercion, `multi m(Any $x, Any $y)` beats
    /// `multi m(Int $x, *@r)` for
    /// `m(1, 2)` whatever their declaration order, while
    /// `multi a(Numeric $g, Numeric $e, $d = '')` and
    /// `multi a(Numeric $g, Numeric $e, Numeric $t, $d = '')` are
    /// incomparable, so which one Test's `is-approx(5, 6, 1)` reaches is
    /// decided by the required-named candidates that sit between them.
    // Cost: O(r), r = the cost of one `conforms` call.
    fn rakudo_narrower(&self, other: &MultiRank, conforms: &dyn Fn(Symbol, Symbol) -> bool) -> bool {
        let coercive_mismatch = self.arity_mismatch(other)
            && self.profile.shares_coercion_with(&other.profile, self.arity.0.min(other.arity.0));
        if self.arity_mismatch(other) && !coercive_mismatch {
            return self.non_slurpy && !other.non_slurpy;
        }
        let narrower = self.narrower_by_rank(other, conforms);
        if narrower || !coercive_mismatch {
            return narrower;
        }
        narrower || (self.rank_cmp(other) == Ordering::Equal && self.decl_order < other.decl_order)
    }

    /// Whether `self` is strictly narrower than `other`: in an earlier tier,
    /// or in the same one and ahead of it in the tier order while comparable
    /// with it. Within a tier, an arity mismatch makes the non-slurpy one of
    /// a slurpy/non-slurpy pair narrower and leaves any other pair
    /// incomparable, except when their shared positional prefix contains a
    /// coercion type. Untiered ranks keep the rank order.
    // Cost: O(r), r = the cost of one `conforms` call.
    pub(crate) fn is_narrower(
        &self,
        other: &MultiRank,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        if self.tier != other.tier {
            return self.tier < other.tier;
        }
        if self.arity_mismatch(other)
            && !self.profile.shares_coercion_with(&other.profile, self.arity.0.min(other.arity.0))
        {
            if self.non_slurpy != other.non_slurpy {
                return self.non_slurpy;
            }
            // Rakudo's incomparable pair. Only a tiered rank can afford it:
            // the tiers already ordered the pair through any third candidate
            // between them, while an untiered caller (multi methods) has
            // nothing but the rank order to go by.
            if self.tiered && other.tiered {
                return false;
            }
        }
        self.narrower_by_rank(other, conforms)
    }

    /// Whether a narrowest-first scan that already bound a candidate ranked
    /// `best` may skip `self` without trying it: `self` is strictly wider and
    /// comparable with `best`. One that is incomparable with it may still win
    /// by declaration order (#11943), so it is tried.
    // Cost: O(r), r = the cost of one `conforms` call.
    pub(crate) fn scan_may_skip(
        &self,
        best: &MultiRank,
        conforms: &dyn Fn(Symbol, Symbol) -> bool,
    ) -> bool {
        best.is_narrower(self, conforms)
    }
}

/// Give each rank its rakudo tier: the candidates nothing in `ranks` is
/// narrower than form tier 0, those only tier-0 candidates are narrower than
/// form tier 1, and so on — rakudo's topological sort of a multi's candidate
/// list. `ranks` is every candidate of one dispatch (whether or not it binds
/// the call), so a pair the relation calls incomparable can still land in two
/// tiers because of a third candidate between them (#12795). A cycle, which
/// the relation can form across an arity mismatch, closes the remaining
/// candidates into one tier.
// Cost: O(n^2 * r), n = candidates, r = the cost of one `conforms` call.
pub(crate) fn assign_tiers<T>(
    items: &mut [T],
    rank: impl Fn(&mut T) -> &mut MultiRank,
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
) {
    let n = items.len();
    if n < 2 {
        for item in items.iter_mut() {
            let rank = rank(item);
            rank.tier = 0;
            rank.tiered = true;
            rank.tier_bind_check = false;
        }
        return;
    }
    let ranks: Vec<MultiRank> = items.iter_mut().map(|item| *rank(item)).collect();
    // A candidate whose arguments are unrelated to its parameter types
    // cannot bind them, and its rank is then made of "unrelated" distances,
    // not of its declared types — the types rakudo's tiers are built from.
    // It takes no part in the relation: comparing it would let an
    // argument-dependent artifact push a binding candidate into a later
    // tier (`multi w(1, 'em')` putting `multi w($f, 'khz')` behind
    // `multi w(Numeric $n, Str:D $u)` for `w(0.1, 'khz')`).
    let narrower: Vec<Vec<bool>> = (0..n)
        .map(|i| {
            (0..n)
                .map(|j| {
                    if i == j {
                        return false;
                    }
                    if ranks[i].profile.unrelated || ranks[j].profile.unrelated {
                        // Only method candidates compare by declared type
                        // (rakudo's tiers do); a sub's unbindable candidate
                        // stays out of the relation.
                        return ranks[i].invocant.is_some()
                            && ranks[j].invocant.is_some()
                            && ranks[i].declared_narrower(&ranks[j], conforms);
                    }
                    ranks[i].rakudo_narrower(&ranks[j], conforms)
                })
                .collect()
        })
        .collect();
    let mut tier_of: Vec<Option<u32>> = vec![None; n];
    let mut tier = 0u32;
    let mut left = n;
    while left > 0 {
        let layer: Vec<usize> = (0..n)
            .filter(|&j| {
                tier_of[j].is_none()
                    && !(0..n).any(|i| tier_of[i].is_none() && narrower[i][j])
            })
            .collect();
        let layer = if layer.is_empty() {
            (0..n).filter(|&j| tier_of[j].is_none()).collect()
        } else {
            layer
        };
        for &j in &layer {
            tier_of[j] = Some(tier);
        }
        left -= layer.len();
        tier += 1;
    }
    let mut tier_has_bind_check = vec![false; tier as usize + 1];
    for (r, t) in ranks.iter().zip(&tier_of) {
        // A candidate whose parameter types cannot take the arguments at all
        // (`Str $s where "x"` for an Int) is not in the trial-bind pool.
        if r.arity_fits && !r.profile.unrelated && (r.nominal.0 > 0 || r.refinement[0] > 0) {
            tier_has_bind_check[t.unwrap_or(0) as usize] = true;
        }
    }
    for (item, tier) in items.iter_mut().zip(tier_of) {
        let rank = rank(item);
        rank.tier = tier.unwrap_or(0);
        rank.tiered = true;
        rank.tier_bind_check = tier_has_bind_check[rank.tier as usize];
    }
}

/// Drop the matches another match is narrower than and, when what is left is
/// split by incomparable candidates, keep only the first declared of them. A
/// pure tie (every survivor tier-equal) is returned whole, in the input
/// order, so the caller's ambiguity logic still sees it.
///
/// The flag is set when the survivors are an incomparable split that no
/// bind-time check can settle (see [`split_ambiguous`]): rakudo raises
/// `X::Multi::Ambiguous` there, so they are all returned for the caller to
/// report.
// Cost: O(m^2 * r), m = matches, r = the cost of one `conforms` call.
pub(crate) fn prune_to_maximal<T>(
    matches: Vec<(MultiRank, T)>,
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
) -> (Vec<(MultiRank, T)>, bool) {
    if matches.len() < 2 {
        return (matches, false);
    }
    let ranks: Vec<MultiRank> = matches.iter().map(|(r, _)| *r).collect();
    let dominated = |k: &MultiRank| ranks.iter().any(|o| o.is_narrower(k, conforms));
    if ranks.iter().all(dominated) {
        // The arity-mismatch rule makes the relation non-transitive, so a
        // cycle can dominate every match; leave the choice to the tier order.
        return (matches, false);
    }
    let mut maximal: Vec<(MultiRank, T)> =
        matches.into_iter().filter(|(k, _)| !dominated(k)).collect();
    let first = maximal[0].0;
    if maximal
        .iter()
        .all(|(k, _)| k.tier_cmp(&first) == Ordering::Equal)
    {
        return (maximal, false);
    }
    let survivors: Vec<MultiRank> = maximal.iter().map(|(k, _)| *k).collect();
    if split_ambiguous(&survivors, conforms) {
        return (maximal, true);
    }
    // Incomparable survivors: declaration order decides.
    let keep = maximal
        .iter()
        .enumerate()
        .min_by_key(|(_, (k, _))| k.decl_order)
        .map(|(i, _)| i)
        .unwrap_or(0);
    (vec![maximal.swap_remove(keep)], false)
}

/// Whether incomparable `survivors` are split by an arity mismatch or by an
/// optional positional the call does not reach (one candidate is narrower
/// there, the other elsewhere, see [`RankProfile::unreached_wins`]), with no
/// bind-time check among them: no `where`, `subset` or literal refinement
/// and no named parameter. Rakudo trial-binds such a check in
/// declaration order, so the first declared that binds wins
/// (`multi b(Int $g where * > 0, Int $e, $d = '')` over
/// `multi b(Any $g, Any $e, Any $t, $d = '')` for `b(5, 6, 1)`); without one,
/// two binding candidates in one tier are ambiguous
/// (`multi a(Int $g, Int $e, $d = '')` / `multi a(Any $g, Any $e, Any $t,
/// $d = '')` for `a(5, 6, 1)`; `multi f(Int $a, $b?)` / `multi f($a, Int $b?)`
/// for `f(1)`).
// Cost: O(s^2 * r), s = survivors, r = the cost of one `conforms` call.
fn split_ambiguous(survivors: &[MultiRank], conforms: &dyn Fn(Symbol, Symbol) -> bool) -> bool {
    survivors.len() > 1
        && survivors
            .iter()
            .all(|k| k.tiered && !k.bind_check && !k.profile.refined && k.refinement[0] == 0)
        && survivors.iter().enumerate().all(|(i, a)| {
            survivors[i + 1..]
                .iter()
                .all(|b| {
                    (a.arity_mismatch(b) && a.non_slurpy == b.non_slurpy)
                        || a.profile.unreached_wins(&b.profile, conforms) != (false, false)
                })
        })
}

/// Whether the candidate declares at least one *explicit* named parameter
/// (a slurpy `*%rest` does not count — it accepts nameds without declaring
/// any). `params` are the candidate's dispatch-visible parameters, the
/// invocant excluded.
///
/// Rakudo treats this as a boolean narrowness step, not a count: a
/// candidate that declares nameds is narrower than one that declares none
/// (`multi b(:$x)` beats `multi b()` for `b()`, in either declaration
/// order), but between two candidates that both declare nameds, *how many*
/// they declare is irrelevant and declaration order decides
/// (`multi K($i, :$a, :$b)` vs `multi K($i, :$a, :$b, :$c)` for
/// `K(1, :a, :b)`). Both verified against rakudo via `&f.cando`.
///
/// It ranks BELOW positional type narrowness and type distance — rakudo
/// picks `multi q(Int $a)` over `multi q(Cool $a, :$x)` for `q(1)` — and
/// ABOVE the optional-positional count, so `multi r($a?, :$x)` beats
/// `multi r($a)` for `r(1)`.
// Cost: O(p), p = parameters.
pub(crate) fn declares_named(params: &[&ParamDef]) -> bool {
    params
        .iter()
        .any(|p| p.named && !p.slurpy && !p.double_slurpy)
}

/// Whether the candidate declares a slurpy *positional* parameter
/// (`*@a`, `**@a`, `+@a`) — rakudo's `max_arity == SLURPY_ARITY`. A slurpy
/// hash takes only nameds and does not count.
// Cost: O(p), p = parameters.
pub(crate) fn has_slurpy_positional(params: &[&ParamDef]) -> bool {
    params
        .iter()
        .any(|p| !p.named && p.is_variadic() && !p.name.starts_with('%'))
}

/// Whether binding the candidate needs a check on the *named* side: it
/// declares an explicit named parameter (see [`declares_named`]) or puts a
/// `where` clause on a slurpy. This is rakudo's `bind_check` flag for the
/// named half of a signature, a boolean narrowness step below every
/// positional one.
///
/// A `where` on a slurpy is not a positional refinement (it is excluded
/// from the refinement tier's constrained count): rakudo ties
/// `multi f(*%m where .elems == 1)` with `multi f(:x($y)!)` and runs the
/// one declared first, while either beats a bare `multi f(*%m)`
/// (CSS::Properties' `measure` multis,
/// [#10519](https://github.com/tokuhirom/mutsu/issues/10519)).
// Cost: O(p), p = parameters.
pub(crate) fn needs_named_bind_check(params: &[&ParamDef]) -> bool {
    declares_named(params)
        || params
            .iter()
            .any(|p| p.is_variadic() && p.where_constraint.is_some())
        // A destructuring sub-signature on a positional (`% (:$op!)`,
        // `($a, $b)`) is checked by trial binding too: a tie with a candidate
        // that lacks one runs the first declared instead of being ambiguous.
        || params.iter().any(|p| !p.named && p.sub_signature.is_some())
}

/// `(positional count, required positional count)` of a candidate's
/// dispatch-visible parameters, slurpies excluded: [`MultiRank::arity`].
// Cost: O(p), p = parameters.
pub(crate) fn positional_arity(params: &[&ParamDef]) -> (usize, usize) {
    let positional = params.iter().filter(|p| !p.named && !p.is_variadic());
    let required = positional
        .clone()
        .filter(|p| !(p.optional_marker || p.default.is_some()))
        .count();
    (positional.count(), required)
}

/// Whether the candidate declares an *optional* positional parameter — an
/// optional or defaulted positional the caller may omit, which makes the
/// candidate accept more calls and therefore LESS specific. Raku ranks a
/// candidate without one as narrower (`multi f(Int:D)` beats
/// `multi f(Int $z?)` for `f(42)`; `multi m(Int $x)` beats
/// `multi m(Int $x, Int $y?)` for `m(1)`). It is a boolean step, not a count:
/// between two candidates that both declare optionals, how many they declare
/// is irrelevant, and rakudo reports `multi m($x?)` / `multi m($x?, $y?)` for
/// `m()` (and `m(1)`) as ambiguous (#12852). Used as a tiebreaker AFTER type
/// distance (a narrower type still wins even when it is the optional one —
/// `f(Int $y?)` beats `f(Cool $x)` for `f(42)`), and as the last step before
/// declaration order. Once both candidates declare named parameters,
/// however, an omitted optional positional does not make one candidate
/// narrower: `multi f($x = 1, :$a)` and `multi f(:$b)` tie and declaration
/// order decides; the named-parameter step already decides a mixed pair.
///
/// A slurpy does not count (the non-slurpy tier ranks it first), and neither
/// does anything in a candidate that needs a named bind check (an explicit
/// named parameter, or a `where` on a slurpy): rakudo ties `multi j($a?, $c?,
/// *%h where .elems == 0)` with `multi j($b?, :$x)` for `j(1)`, and the first
/// declared wins (#12774).
// Cost: O(p), p = parameters.
pub(crate) fn has_optional_positional(params: &[&ParamDef]) -> bool {
    !needs_named_bind_check(params)
        && params
            .iter()
            .filter(|p| !p.named && !p.slurpy && !p.double_slurpy)
            .any(|p| p.optional_marker || p.default.is_some())
}
