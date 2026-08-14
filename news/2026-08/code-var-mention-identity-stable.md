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

## One consumer DID need a guard: `closure_captured_state`

Every other subsystem keying off `SubData::id` directly (`wrap_chains`, `protect_block_cache`,
`nested_react_callbacks`, `eval_block_value_cached`) inherits the corrected scope for free — no
changes needed. `closure_captured_state` is the one exception, caught by CI's `test`/`gc-stress`/
`jit-stress` jobs on the first push: `call_compiled_closure_with_topic` persists a free variable a
closure body *writes* into `closure_captured_state`, keyed by `data.id`, and replays that persisted
value on the next call sharing that id — correct for a genuine per-clone closure factory (two
closures from the same factory keep independent state), but wrong once `data.id` is stabilized
across repeated bareword mentions of the SAME routine: a captured variable reassigned from
*outside* the routine between two mentions (`$runs = 0;` in `roast/S03-metaops/hyper.t`'s `sub
elems is nodal { $runs⚛++; ... }`, dispatched via `».&elems`) was silently ignored by the next
call, which replayed the stale persisted value instead of the live (just-reset) one — a real,
deterministic regression (`hyper.t` test 408, call count `2, 4, 6, 8` instead of `2, 2, 2, 2`).

The fix restricts stabilization to routines whose compiled body writes NO free variable
(`CompiledCode::free_var_writes` and `free_var_container_writes` both empty; a def with no
`compiled_routine` at all conservatively skips stabilization too). A routine that only *reads* a
captured free var is unaffected by `closure_captured_state` (it is gated on writes) and still gets
a stable identity; a routine that writes one keeps today's fresh-mint-per-mention behavior, so the
`.wrap()`-through-a-different-mention fix above only applies to non-capturing-write routines (the
common case, including the ticket's own repro).

Pinned by the new `t/code-var-mention-identity-stable.t` (11 assertions, cross-checked against real
`raku`), including two that reproduce the `hyper.t` shape directly (a `».&`-dispatched routine that
writes a captured `atomicint`, reset between two dispatch rounds) to guard against this exact
regression recurring. `make test`, the roast `hyper.t`/`wrap.t`/`state.t` suites (release build,
`MUTSU_FUDGE=1`), and `cargo clippy`/`fmt` all stay green, including under `MUTSU_GC=on
MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`.
