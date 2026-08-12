# Multi candidate matcher admits Int for a `Num $x` parameter; raku rejects at dispatch

Found by the ADR-0019 E9-pre follow-up probes (2026-08-12, Rakudo v2026.06).

## Divergence

```raku
class X1 { multi method m(Num $x) { say "num" } }
X1.new.m(1);
# raku:  dies X::Multi::NoMatch — the dispatcher's filter rejects an Int argument
#        for a Num parameter (1 !~~ Num; Num means floating point, not Numeric)
# mutsu: the candidate MATCHER accepts it, dispatch selects the candidate, then the
#        binder dies X::TypeCheck::Binding::Parameter ("expected Num, got Int")
```

So mutsu's matcher and binder disagree about Int-vs-Num: the matcher applies a numeric
leniency the binder (correctly) does not. The user-visible exception type is wrong
(X::TypeCheck::Binding::Parameter instead of X::Multi::NoMatch), and worse, in a deferral
chain the over-admission KILLS the walk mid-flight: E9-pre model probe 2 (`nextsame` through a
3-level hierarchy whose child candidate is `Num $x`, inlined in the E9 redrawn-decision-2
section of `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`) dies at the second step
in mutsu where raku simply skips the Num candidate and completes the chain.

## Where to look

The method-multi match ladder (`method_candidate_type_distance` / the `narrowness` tuple,
`src/runtime/resolution_method.rs`) and/or `method_args_match_for_invocant` — wherever an Int
argument is scored as compatible with a `Num` type constraint. Sub multis share the risk via
their own hand-synchronized ladder (`candidate_specificity_rank`/`candidate_type_distance`,
`src/runtime/dispatch_candidates.rs`) — check `multi sub f(Num $x) {}; f(1)` too (raku: also
X::Multi::NoMatch).

Scope the fix carefully: `Num` must not match Int/Rat arguments, but `Numeric`/`Real`/`Cool`
must keep matching them, and literal `Num` arguments (`1e0`) must keep matching `Num`. Also
verify coercion types (`Num() $x`) still accept Int — only the plain nominal `Num` constraint
is affected.

## Why it matters for ADR-0019 E9

The redrawn E9 cursor advances through the deferral expansion applying a per-call signature
filter; that filter must be raku-strict or the cursor will invoke candidates raku skips (and
die in the binder, as probe 2 shows). Listed there as a prerequisite/co-requisite for E9a.
