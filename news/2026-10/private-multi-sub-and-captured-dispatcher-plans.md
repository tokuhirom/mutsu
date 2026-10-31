# Module-private multi subs and captured dispatchers are planned once, not re-ranked per call

A call to a module's `my multi sub` family (`Array::Sorted::Util`'s `inserts`, `finds`,
`insert`, #15786) rebuilt the dispatch plan on every call: the compunit-private name
withheld it from the plan cache, a named argument (`:force`) made the by-name key refuse
the call, and the `my proto sub inserts(|) is export {*}` dispatcher value then walked its 22 captured
candidates per call (a `FunctionDef` and a body fingerprint each, then rank, sort, bind).

Now (ADR-15737, `multi_dispatch_plan.rs`):

- a compunit-scoped name is planned per asking unit (the executing units join the key, as they
  already did for the plain-sub resolve memo);
- the unkeyed entry keys a named argument by name and value type, as the keyed callers already do;
- a captured dispatcher's winner and `callsame`/`nextsame` chain come from a plan cached per
  (captured candidate ids, argument types); a call that cannot be keyed keeps the per-call walk.

Measured with the profiling build against the main build taken just before, on the
`inserts @k, $_ % 5, @v, $_, :force` loop (Ir per call, slope between 40 and 120 calls):
8,309,191 -> 4,119,718 (-50%). The issue's own loop (1000 calls, `% 50`, wall, same box):
1.47 s -> 0.85 s; rakudo 0.11 s, so this is still ~7.7x rakudo (13x before), not yet the 3x of
#15786's close condition.
