//! The native `infix:<...>` implementations as multi-dispatch candidates.
//!
//! This is the infix half of [ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md):
//! in Raku an operator is a `multi`, so declaring `multi infix:<+>($a, $b)`
//! *joins* `&infix:<+>`'s candidate set instead of replacing the operator.
//! mutsu implements the operator natively, so there was no candidate for the
//! user's `($a, $b)` to lose a narrowness comparison against and it won for
//! every argument type — `multi infix:<+>($a, $b) is default { "USER" }; say 1 + 2`
//! printed `USER` where rakudo prints `3`.
//!
//! The core candidate sets modelled here are transcribed from rakudo's own
//! `&infix:<op>.candidates` (measured 2026-09-07), reduced to the two
//! *positional* type constraints of each two-operand candidate. Only the
//! constraints matter: the ranking never runs a core candidate, it only decides
//! whether the user's candidate is narrower than the narrowest core one. When
//! the core set wins, `try_user_infix` reports "no user candidate" and the
//! native implementation runs exactly as it did before.
//!
//! Ranking reuses the metrics ordinary multi dispatch ranks by
//! ([`Interpreter::candidate_specificity_rank_for_args`] and
//! [`Interpreter::candidate_type_distance`]), so an operator ranks by the same
//! rules as any other multi, and — as in ADR-0071 — **ties go to core**.

use super::*;
use crate::runtime::dispatch_candidates::UNRELATED_DISTANCE;

/// One positional parameter of a core candidate: a nominal type constraint,
/// optionally with a `:D` smiley. `Mu` stands for rakudo's raw `\a` parameter.
type CoreParam = &'static str;

/// One core candidate, reduced to its two positional constraints.
pub(crate) type CoreSig = (CoreParam, CoreParam);

// --- The building blocks the core candidate sets are assembled from ---------
//
// The names are rakudo's own. `Rational` is the role `Rat`/`FatRat` do, so a
// user candidate typed `Rat` out-narrows the core `(Rational:D, Rational:D)`
// row; rakudo's `Num(Real)` coercion parameter accepts exactly `Real`.

const INT_NUM: &[CoreSig] = &[("Int:D", "Int:D"), ("Num:D", "Num:D")];
const RATIONAL: &[CoreSig] = &[
    ("Rational:D", "Rational:D"),
    ("Rational:D", "Int:D"),
    ("Int:D", "Rational:D"),
];
const RATIONAL_POW: &[CoreSig] = &[("Rational:D", "Int:D")];
const COMPLEX: &[CoreSig] = &[
    ("Complex:D", "Complex:D"),
    ("Complex:D", "Real"),
    ("Real", "Complex:D"),
];
const REAL_PAIR: &[CoreSig] = &[("Real", "Real")];
const REAL_D_PAIR: &[CoreSig] = &[("Real:D", "Real:D")];
/// The `Instant`/`Duration`/`Date`/`DateTime` candidates `infix:<+>` and
/// `infix:<->` carry. `Instant` and `Duration` do `Real` in rakudo, but mutsu's
/// `type_hierarchy_distance` has no MRO for them, so they are listed explicitly.
const TEMPORAL: &[CoreSig] = &[
    ("Instant:D", "Instant:D"),
    ("Instant:D", "Real:D"),
    ("Real:D", "Instant:D"),
    ("Instant:D", "Duration:D"),
    ("Duration:D", "Instant:D"),
    ("Duration:D", "Duration:D"),
    ("Duration:D", "Real"),
    ("Real", "Duration:D"),
    ("DateTime:D", "Duration:D"),
    ("Duration:D", "DateTime:D"),
    ("Date:D", "Int:D"),
    ("Int:D", "Date:D"),
];
const DURATION_MOD: &[CoreSig] = &[("Duration:D", "Real")];
const RANGE_REAL: &[CoreSig] = &[("Range:D", "Real:D"), ("Real:D", "Range:D")];
const INSTANT_VERSION: &[CoreSig] = &[("Instant:D", "Instant:D"), ("Version:D", "Version:D")];
const STR_PAIR: &[CoreSig] = &[("Str:D", "Str:D")];
const BLOB_PAIR: &[CoreSig] = &[("Blob:D", "Blob:D")];
/// `infix:<~>`'s core set: `Str:D`/`Cool:D`/`Any:D` mixes. Its only catch-all
/// is a slurpy `(*@args)`, which every two-positional user candidate beats.
const CONCAT: &[CoreSig] = &[
    ("Str:D", "Str:D"),
    ("Cool:D", "Str:D"),
    ("Str:D", "Cool:D"),
    ("Cool:D", "Cool:D"),
    ("Any:D", "Str:D"),
    ("Str:D", "Any:D"),
];
/// `infix:<cmp>` has by far the largest core set; these are the rows that can
/// beat a user candidate. `(Real:D $a, \b)` and `(\a, Real:D $b)` are the
/// half-typed rows, which is why `cmp` needs a per-operand typed count.
const CMP_EXTRA: &[CoreSig] = &[
    ("Pair:D", "Pair:D"),
    ("List:D", "List:D"),
    ("Range:D", "Range:D"),
    ("Real:D", "Mu"),
    ("Mu", "Real:D"),
];

/// The core candidate set of one natively-implemented infix operator.
pub(crate) struct CoreInfixShape {
    /// The typed two-positional candidates, as groups so operator families can
    /// share them.
    pub(crate) sigs: &'static [&'static [CoreSig]],
    /// The constraint of the operator's two-positional catch-all candidate
    /// (rakudo's `(\a, \b)` is `Mu`), or `None` when the operator has none —
    /// `infix:<~>`'s widest candidate is a slurpy, which any two-positional
    /// user candidate out-narrows.
    pub(crate) catch_all: Option<CoreParam>,
}

/// Does `infix:<op>` have a core candidate set at all, and does mutsu model it?
///
/// One notion, three answers — the classification
/// [#8006](https://github.com/tokuhirom/mutsu/issues/8006) asked for. Before
/// it, two separate answers disagreed: [`core_infix_shape`] returning `None`
/// meant "no core candidate to rank", which the ranking read as "the user
/// candidate wins", while `call_infix_fallback` went on running the builtin
/// anyway when no user candidate matched. For [`Self::Shadowing`] those two
/// readings are now the same one: there is no core candidate, in either
/// direction.
pub(crate) enum CoreInfixCandidates {
    /// rakudo declares `&infix:<op>` and mutsu models the type constraints of
    /// its two-positional candidates. A user candidate joins the set and has to
    /// out-narrow the core one to take the call.
    Modelled(CoreInfixShape),
    /// rakudo declares `&infix:<op>`, but mutsu does not model its candidate
    /// types. A matching user candidate takes the call; when none matches, the
    /// native implementation still answers — which is what rakudo's own core
    /// candidates do.
    Unmodelled,
    /// rakudo has **no** `&infix:<op>` routine: mutsu's infix spelling of the
    /// name is a convenience over a core *list-op sub* of the same bare name
    /// (`cross`, `zip`, `roundrobin`, ...), or the operator is purely
    /// user-defined (`infix:<@@>`). Declaring a routine of this name therefore
    /// installs a fresh lexical routine that SHADOWS whatever mutsu answered
    /// before: a call its candidates do not accept is `X::Multi::NoMatch`, not
    /// a fall-through to the builtin.
    Shadowing,
}

/// Classify `name` (`"infix:<+>"`). The bare-name form is not accepted — every
/// caller holds the `infix:<...>` spelling the dispatcher keys on.
pub(crate) fn core_infix_candidates(name: &str) -> CoreInfixCandidates {
    let Some(op) = name
        .strip_prefix("infix:<")
        .and_then(|s| s.strip_suffix('>'))
    else {
        return CoreInfixCandidates::Shadowing;
    };
    match core_infix_shape(name) {
        Some(shape) => CoreInfixCandidates::Modelled(shape),
        None if crate::runtime::core_infix_names::rakudo_declares_infix(op) => {
            CoreInfixCandidates::Unmodelled
        }
        None => CoreInfixCandidates::Shadowing,
    }
}

/// The modelled core candidate set for `name` (`"infix:<+>"`), or `None` when
/// mutsu has no type table for the operator — which is both of the other two
/// [`CoreInfixCandidates`] answers, so prefer that function unless the shape
/// itself is what is wanted.
fn core_infix_shape(name: &str) -> Option<CoreInfixShape> {
    let op = name.strip_prefix("infix:<")?.strip_suffix('>')?;
    let (sigs, catch_all): (&'static [&'static [CoreSig]], Option<CoreParam>) = match op {
        "+" | "-" => (
            &[INT_NUM, RATIONAL, COMPLEX, REAL_PAIR, TEMPORAL, RANGE_REAL],
            Some("Mu"),
        ),
        "*" | "/" => (
            &[INT_NUM, RATIONAL, COMPLEX, REAL_PAIR, RANGE_REAL],
            Some("Mu"),
        ),
        "**" => (&[INT_NUM, RATIONAL_POW, COMPLEX, REAL_PAIR], Some("Mu")),
        "%" => (&[INT_NUM, RATIONAL, REAL_PAIR, DURATION_MOD], Some("Mu")),
        "==" | "<=>" => (
            &[INT_NUM, RATIONAL, COMPLEX, REAL_PAIR, INSTANT_VERSION],
            Some("Mu"),
        ),
        "<" | "<=" | ">" | ">=" => (&[INT_NUM, RATIONAL, REAL_PAIR, INSTANT_VERSION], Some("Mu")),
        // `infix:<!=>` is the thinnest of the numeric family: rakudo gives it
        // `Int:D`/`Num:D`/`Instant:D`/`Version:D` pairs and a `(Mu \a, Mu \b)`
        // catch-all, with no `Real` or `Rational` rows.
        "!=" => (&[INT_NUM, INSTANT_VERSION], Some("Mu")),
        "%%" => (&[&[("Int:D", "Int:D")]], Some("Mu")),
        "div" => (&[&[("Int:D", "Int:D")]], Some("Any")),
        "mod" => (&[REAL_D_PAIR], Some("Any")),
        // `gcd`/`lcm` reduce their operands to `Int`: rakudo's two-positional
        // candidates are `(Int:D, Int:D)` and the `(\a, \b)` catch-all.
        "gcd" | "lcm" => (&[&[("Int:D", "Int:D")]], Some("Mu")),
        // `min`/`max` add `(Num:D, Num:D)` to that, and their catch-all is
        // `(Mu:D \a, Mu:D \b)`.
        "min" | "max" => (&[INT_NUM], Some("Mu")),
        "~" => (&[CONCAT, BLOB_PAIR], None),
        "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "leg" => (&[STR_PAIR, BLOB_PAIR], Some("Mu")),
        "cmp" => (
            &[
                INT_NUM,
                RATIONAL,
                COMPLEX,
                REAL_D_PAIR,
                STR_PAIR,
                BLOB_PAIR,
                INSTANT_VERSION,
                CMP_EXTRA,
            ],
            Some("Mu"),
        ),
        _ => return None,
    };
    Some(CoreInfixShape { sigs, catch_all })
}

/// The narrowness of one core candidate for the call being ranked: how many of
/// its two positionals carry a *meaningful* type (mirroring
/// `candidate_specificity_rank_for_args`, which does not count a bare
/// `Mu`/`Any`), and the summed MRO distance from the arguments.
/// What [`Interpreter::core_infix_candidate_wins`] is memoized on, besides the
/// functions map (whose generation tags the entry). `def` is the candidate's
/// address; the entry holds the `Arc` too, so a hit is confirmed by pointer
/// identity and an address can never be reused while the entry lives.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct CoreInfixWinsKey {
    name: Symbol,
    def: usize,
    proto_generation: u64,
    subsets: usize,
    arg_keys: Vec<Symbol>,
}

#[derive(Clone, Copy)]
struct CoreRank {
    typed: usize,
    distance: usize,
    /// The two positional constraints of the candidate, for the ambiguity message.
    sig: CoreSig,
}

/// Who takes a call to an operator that has both a user candidate and the
/// modelled core set.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum CoreInfixVerdict {
    /// The user candidate out-ranks the core set (or there is no core
    /// candidate to rank against).
    User,
    /// The core set out-ranks the user candidate: the native operator runs.
    Core,
    /// The user candidate ties the narrowest core candidate exactly and is not
    /// `is default`: `X::Multi::Ambiguous` in rakudo.
    Tie(CoreSig),
}

impl Interpreter {
    /// Does the core candidate set of `name` out-rank the user candidate `def`
    /// for the operands `left`/`right`?
    ///
    /// `false` means the user candidate takes the call, which is what
    /// `try_user_infix` did unconditionally before. Callers must have already
    /// resolved `def` — the question only arises once a user candidate matched.
    ///
    /// A plain `sub infix:<op>` (not a `multi`) is a lexical shadow rather than
    /// a candidate: it replaces the operator outright, as in rakudo, so this
    /// answers `false` for it.
    ///
    /// The answer is a function of the operand *types*, so it is memoized per
    /// `(operator, candidate, argument type keys)` in
    /// [`ResolutionCaches::core_infix_wins_cache`](crate::runtime::resolution_caches::ResolutionCaches::core_infix_wins_cache) (#10111): ranking the core set
    /// walks every modelled core signature through `type_hierarchy_distance`,
    /// ~30 string-keyed MRO walks per operator call, which was the largest
    /// single cost of a user `multi infix:<*>` call. A candidate whose rank
    /// reads an argument's value (a type capture) is never memoized.
    // Cost: O(k) on a hit, k = operands (the key build); O(s · d) on a miss,
    // s = modelled core signatures, d = MRO depth of the operand types.
    pub(crate) fn core_infix_candidate_wins(
        &mut self,
        name: &str,
        def: &Arc<FunctionDef>,
        left: &Value,
        right: &Value,
    ) -> CoreInfixVerdict {
        let key = self.core_infix_wins_key(name, def, left, right);
        let generation = self.caches.fn_resolve_gen;
        if let Some(key) = &key
            && let Some((cached_def, wins)) = self.caches.core_infix_wins_cache.get(generation, key)
            && Arc::ptr_eq(cached_def, def)
        {
            return *wins;
        }
        let wins = self.core_infix_candidate_wins_uncached(name, def, left, right);
        if let Some(key) = key {
            debug_assert_eq!(generation, self.caches.fn_resolve_gen);
            self.caches
                .core_infix_wins_cache
                .insert(generation, key, (Arc::clone(def), wins));
        }
        wins
    }

    /// The memo key for [`Self::core_infix_candidate_wins`], or `None` when the
    /// answer must be computed fresh: an operand that does not reduce to a type
    /// key (a `Junction`, a mixin, a container), or a candidate whose rank reads
    /// an argument's value. The registry state the ranking reads besides the
    /// functions map — the proto generation and the registered subsets — joins
    /// the key the way it joins `BareMultiPlanKey`; the functions map itself is
    /// the cache generation.
    fn core_infix_wins_key(
        &mut self,
        name: &str,
        def: &Arc<FunctionDef>,
        left: &Value,
        right: &Value,
    ) -> Option<CoreInfixWinsKey> {
        if self.candidate_rank_reads_value(def) {
            return None;
        }
        let arg_keys = self.multi_arg_type_keys(&[left.clone(), right.clone()])?;
        Some(CoreInfixWinsKey {
            name: Symbol::intern(name),
            def: Arc::as_ptr(def) as usize,
            proto_generation: self.registry().proto_generation(),
            subsets: self.registry().subsets.len(),
            arg_keys,
        })
    }

    fn core_infix_candidate_wins_uncached(
        &mut self,
        name: &str,
        def: &FunctionDef,
        left: &Value,
        right: &Value,
    ) -> CoreInfixVerdict {
        if !self.has_multi_function_cached(name) {
            return CoreInfixVerdict::User;
        }
        self.core_infix_verdict(name, def, left, right)
    }

    /// [`Self::core_infix_candidate_wins`] for a user candidate the registry
    /// does not hold: one of the candidates of a built-in operator that a
    /// block-scoped `use` imported (ADR-8751 stage 2).
    // Cost: as `core_infix_candidate_wins` uncached.
    pub(crate) fn core_infix_verdict(
        &mut self,
        name: &str,
        def: &FunctionDef,
        left: &Value,
        right: &Value,
    ) -> CoreInfixVerdict {
        let CoreInfixCandidates::Modelled(shape) = core_infix_candidates(name) else {
            // Neither an unmodelled core operator nor a shadowing declaration
            // has a core candidate to rank against, so the user's takes the
            // call. What differs between the two is what happens when NO user
            // candidate matches, and that is `call_infix_fallback`'s question.
            return CoreInfixVerdict::User;
        };
        let Some(core) = self.narrowest_core_infix_candidate(&shape, left, right) else {
            return CoreInfixVerdict::User;
        };
        // Rank the way multi dispatch does, with *nominal type* narrowness as
        // the primary key: a refinement (subset / `where`) only breaks a tie
        // between equally narrow nominal types, so an untyped
        // `($a where * > 0, $b)` still loses to the core `(Int:D, Int:D)` while
        // a `subset` of `Int` still beats it. The core candidates are plain
        // nominal types, so they carry no refinement of any kind.
        let args = [left.clone(), right.clone()];
        let (literal, typed, constrained, subsig, _traits) =
            self.candidate_specificity_rank_for_args(def, &args);
        // A literal parameter (`multi infix:<->(e1, e2)`, roast
        // S03-operators/custom.t) is not merely a refinement: rakudo compiles
        // it to the literal's own type plus an equality constraint, so it is
        // nominally exactly as narrow as the argument and then wins the tie on
        // the refinement. `candidate_specificity_rank_for_args` counts it only
        // in `literal`, leaving `typed` at zero, so add it back here.
        let literal_positionals = def
            .param_defs
            .iter()
            .filter(|p| !p.named && p.literal_value.is_some() && p.type_constraint.is_none())
            .count();
        // A parameter whose type and the core one's are unrelated (a mixed-in
        // role against `Rational`) cannot be ranked by counts and distances:
        // rakudo compares the declared types parameter by parameter.
        if let Some(verdict) = self.core_infix_unrelated_verdict(def, &core) {
            return verdict;
        }
        let user_key = (typed + literal_positionals, literal, constrained, subsig);
        let core_key = (core.typed, 0usize, 0usize, 0usize);
        if user_key != core_key {
            return if core_key > user_key {
                CoreInfixVerdict::Core
            } else {
                CoreInfixVerdict::User
            };
        }
        // Equally narrow by nominal type (`multi infix:<+>(U $a, U $b) is
        // default` against the core `(Int:D, Int:D)` for `1 but U`), `is
        // default` is what breaks the tie in the user candidate's favour; an
        // unmarked one is ambiguous in rakudo (#12973).
        if def.is_default {
            return CoreInfixVerdict::User;
        }
        let user_distance = self.candidate_type_distance(&args, def);
        match core.distance.cmp(&user_distance) {
            std::cmp::Ordering::Less => CoreInfixVerdict::Core,
            std::cmp::Ordering::Equal => CoreInfixVerdict::Tie(core.sig),
            std::cmp::Ordering::Greater => CoreInfixVerdict::User,
        }
    }

    /// Rakudo's parameter-wise comparison of a user candidate against the
    /// narrowest core one, for the case the count-and-distance key cannot
    /// rank: some positional parameter's declared type is *unrelated* to the
    /// core candidate's (`multi infix:<*>(U $v, Numeric $n)` against the core
    /// `(Rational:D, Int:D)`, for `1.5 but U[...]`). Measured against
    /// rakudo 2026.07: the core candidate wins when the user's type is wider
    /// than the core's in any parameter; otherwise the user's wins when it is
    /// narrower in some parameter; otherwise the pair is a tie (ambiguous,
    /// unless the user candidate is `is default`). Smileys are ignored.
    /// `None` when every pair of types is related: the count-and-distance
    /// key answers then.
    // Cost: O(1) conformance walks (two positional parameters).
    fn core_infix_unrelated_verdict(
        &self,
        def: &FunctionDef,
        core: &CoreRank,
    ) -> Option<CoreInfixVerdict> {
        let params: Vec<&ParamDef> = Self::dispatch_visible_params(def)
            .into_iter()
            .filter(|p| !p.named && !p.slurpy)
            .collect();
        if params.len() != 2 {
            return None;
        }
        let nominal = |constraint: &str| -> Symbol {
            let base = constraint
                .strip_suffix(":D")
                .or_else(|| constraint.strip_suffix(":U"))
                .or_else(|| constraint.strip_suffix(":_"))
                .unwrap_or(constraint);
            Symbol::intern(base)
        };
        let (mut unrelated, mut user_wider, mut user_narrower) = (false, false, false);
        for (param, core_type) in params.iter().zip([core.sig.0, core.sig.1]) {
            // Only a plain nominal type takes part; a literal, coercion or
            // sub-signature parameter leaves the question to the key.
            if param.literal_value.is_some()
                || param.sub_signature.is_some()
                || param.code_signature.is_some()
                || param.type_constraint.as_deref().is_some_and(|t| t.contains(['(', '[']))
            {
                return None;
            }
            let user = param
                .type_constraint
                .as_deref()
                .map_or_else(|| Symbol::intern("Any"), nominal);
            // An enum value or other non-type name used as a parameter
            // constraint is a value match, not a nominal type.
            if let Some(t) = param.type_constraint.as_deref() {
                let base = nominal(t).with_str(str::to_owned);
                if !self.has_type(&base)
                    && !crate::runtime::utils::is_known_type_constraint(&base)
                {
                    return None;
                }
            }
            let core_sym = nominal(core_type);
            let (down, up) = (
                self.nominal_type_conforms(user, core_sym),
                self.nominal_type_conforms(core_sym, user),
            );
            match (down, up) {
                (true, true) => {}
                (true, false) => user_narrower = true,
                (false, true) => user_wider = true,
                (false, false) => unrelated = true,
            }
        }
        let _ = unrelated;
        Some(if user_wider {
            CoreInfixVerdict::Core
        } else if user_narrower || def.is_default {
            CoreInfixVerdict::User
        } else {
            CoreInfixVerdict::Tie(core.sig)
        })
    }

    /// The `X::Multi::Ambiguous` for a user infix candidate that ties the
    /// narrowest core candidate (`multi infix:<%>(Int $a, Int $b)`).
    // Cost: O(p), p = parameters of the user candidate (the message build).
    pub(crate) fn core_infix_tie_error(
        &self,
        name: &str,
        def: &FunctionDef,
        core: CoreSig,
        left: &Value,
        right: &Value,
    ) -> RuntimeError {
        let op = name
            .strip_prefix("infix:<")
            .and_then(|n| n.strip_suffix('>'))
            .unwrap_or(name);
        let arg_types = format!(
            "{}, {}",
            super::value_type_name(&Self::infix_dispatch_probe(left)),
            super::value_type_name(&Self::infix_dispatch_probe(right))
        );
        let user_sig = Self::build_signature_string(&def.param_defs);
        let header = format!("infix:<{}>({})", op, arg_types);
        let message = format!(
            "Ambiguous call to '{}'; these signatures all match:\n  ({} $a, {} $b) from SETTING::src/core.c\n  {}",
            header, core.0, core.1, user_sig
        );
        let mut err = RuntimeError::new(message.clone());
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::Ambiguous"),
            attrs,
        )));
        err
    }

    /// The narrowest core candidate that accepts `left`/`right`, or `None` when
    /// none does (only possible for an operator whose widest candidate is a
    /// slurpy, such as `infix:<~>`).
    fn narrowest_core_infix_candidate(
        &self,
        shape: &CoreInfixShape,
        left: &Value,
        right: &Value,
    ) -> Option<CoreRank> {
        // An allomorph or a role-mixed value (`<42>`, `1 but Foo`) dispatches as
        // its base type; `type_hierarchy_distance` does not look through the
        // wrapper, and `value_type_name` answers `IntStr` for `<42>`, which has
        // no MRO. rakudo runs the core `Int:D` candidate for `<42> + 1`.
        let l = Self::infix_dispatch_probe(left);
        let r = Self::infix_dispatch_probe(right);
        let mut best: Option<CoreRank> = None;
        for group in shape.sigs {
            for &(lc, rc) in *group {
                let Some(ld) = self.core_param_distance(lc, &l) else {
                    continue;
                };
                let Some(rd) = self.core_param_distance(rc, &r) else {
                    continue;
                };
                let rank = CoreRank {
                    sig: (lc, rc),
                    typed: Self::core_param_is_typed(lc) as usize
                        + Self::core_param_is_typed(rc) as usize,
                    distance: ld + rd,
                };
                best = Some(match best {
                    Some(b)
                        if (b.typed, std::cmp::Reverse(b.distance))
                            >= (rank.typed, std::cmp::Reverse(rank.distance)) =>
                    {
                        b
                    }
                    _ => rank,
                });
            }
        }
        if best.is_some() {
            return best;
        }
        let catch_all = shape.catch_all?;
        let ld = self.core_param_distance(catch_all, &l)?;
        let rd = self.core_param_distance(catch_all, &r)?;
        Some(CoreRank {
            sig: (catch_all, catch_all),
            typed: 2 * Self::core_param_is_typed(catch_all) as usize,
            distance: ld + rd,
        })
    }

    /// The value an operand dispatches as, looking through the `VarRef` the
    /// call site may have wrapped it in and through an allomorph / mixin.
    fn infix_dispatch_probe(arg: &Value) -> Value {
        let inner = match arg.as_varref() {
            Some((_, inner, _)) => inner.clone(),
            None => arg.clone(),
        };
        match inner.view() {
            ValueView::Mixin(base, _) => base.as_ref().clone(),
            _ => inner,
        }
    }

    /// How far `value` is from a core parameter's constraint, or `None` when it
    /// does not bind to it at all (wrong type, or a `:D` parameter given a type
    /// object).
    fn core_param_distance(&self, constraint: CoreParam, value: &Value) -> Option<usize> {
        if constraint.ends_with(":D") && !crate::runtime::types::value_is_defined(value) {
            return None;
        }
        let distance = self.type_hierarchy_distance(constraint, value);
        (distance < UNRELATED_DISTANCE).then_some(distance)
    }

    /// Whether a core parameter counts toward narrowness. Mirrors
    /// `candidate_specificity_rank_for_args`: a bare `Mu`/`Any` is no narrower
    /// than an unconstrained parameter, but a smiley (`Any:D`) counts.
    fn core_param_is_typed(constraint: CoreParam) -> bool {
        !matches!(constraint, "Mu" | "Any")
    }
}
