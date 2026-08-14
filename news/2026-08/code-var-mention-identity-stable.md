# A bareword `&f` mention now has a stable identity

```raku
sub f() { 1 }
say &f.WHICH;   # mutsu: was Sub|27, Sub|29 (different each time)  raku: same both times
```

`sub_value_from_function_def` (the routine `resolve_code_var` calls to materialize a `Sub` value
for a bareword `&f` mention) built a brand-new `SubData` — fresh `id`, fresh env snapshot — on
every single mention, so a repeatedly-read `&f` had no stable identity across reads. `&f === &f`
happened to read `True` anyway (`values_identical`'s `Sub` arm falls back to a `(name, package)`
comparison when the `Gc::ptr_eq` check fails), which is why the bug went unnoticed by anything
that only used `===`; `.WHICH`/`.WHERE` inherited the instability directly, since both are
computed straight from `SubData::id`.

## A second, more serious symptom: `.wrap()` silently bypassed for a "wrong" mention

The unstable id was not just cosmetic. `resolution_call_sub.rs`'s direct-value call path looks up
an active wrap chain via `self.wrap_chains.get(&data.id)`. A `.wrap()` installed through one
mention of a routine stored its wrapper chain under THAT mention's id; a later, different mention
of the *same* routine, invoked directly as a value (`&f()`), got a *different* fresh id and so
missed the wrap chain entirely — even though `f()` (an ordinary named call, which resolves the
wrap chain by name instead) saw it correctly. Verified directly:

```raku
sub f() { say "original" }
my &g = &f;
&g.wrap(sub () { say "wrapped"; callsame; });
&f();   # mutsu (before this fix): "original" only — the wrap never fires
        # raku:                    "wrapped" then "original"
f();    # both mutsu and raku: "wrapped" then "original" (named calls already worked)
```

## Fix

The routine's REGISTRATION clone id — the env marker `__mutsu_callable_id::Pkg::name`, refreshed
on every `RegisterSub` execution — already existed and was already trusted for exactly this
granularity of identity, just for a different purpose: `Interpreter::sub_state_scope_id` uses it
to decide which `state`-variable store a call should read/write ("a nested named sub re-initializes
per enclosing call while a top-level sub's state persists"). `sub_value_from_function_def` now
looks up the same marker and, when present, stabilizes the materialized `Sub`'s `id` to it instead
of always minting a fresh one via `next_instance_id()`.

This gives exactly the right lifetime for free, with no new cache/side-table needed:

- A top-level or class-method sub's clone id is set once (its single `RegisterSub` execution) and
  never changes, so `&f.WHICH` is stable for the program's lifetime — matching raku.
- A `sub` nested inside another routine's body re-executes its `RegisterSub` on every invocation of
  the enclosing routine, refreshing the clone id — so `&inner` gets a *fresh* identity per
  invocation, matching raku's own behavior (verified directly against `raku`: a nested `my sub`
  closes fresh per call even when it captures nothing from the enclosing scope — real raku does not
  special-case "no captures" here, so the fix does not either).
- Repeated mentions of the same nested sub *within one invocation* of its enclosing routine share
  one identity, same rule as the top-level case.
- A def with no visible registration record (synthesized, or looked up before its `RegisterSub`
  ran) falls back to the previous fresh-mint behavior, unchanged.

Because every other subsystem that keys off `SubData::id` directly (`wrap_chains`,
`closure_captured_state`, `protect_block_cache`, `nested_react_callbacks`,
`eval_block_value_cached`) now sees a `data.id` with the correct scope baked in, the wrap-chain
bypass above is also fixed as a natural consequence — no changes were needed to any of those
consumers.

Pinned by the new `t/code-var-mention-identity-stable.t` (8 assertions, cross-checked against real
`raku`, covering `.WHICH`/`.WHERE` stability, the wrap-through-a-different-mention case, and the
nested-per-invocation-identity case). `make test` and the roast `wrap.t`/`state.t` suites (release
build, `MUTSU_FUDGE=1`) stay green, including under `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024
MUTSU_GC_VERIFY=1`.
