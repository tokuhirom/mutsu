# A user-defined operator runs its compiled body

ADR-0019 C6d-1, second slice. The first one moved the multi-deferral caller off
the interpreter entry `call_function_def`; this one moves every remaining
caller. `call_function_def`'s body run is `run_block(&def.body)` ->
`run_block_raw` -> `compile_block_raw`: **the routine's AST body was compiled
afresh on every call.** These callers now run the routine's bytecode instead —
the body the declaration plan attached, or one memoized on-the-fly compile per
body.

The callers, all of which had already resolved the exact candidate they wanted:

| caller | what reaches it |
| --- | --- |
| `builtins_operators_fallback` | a user `prefix:<>` / `postfix:<>` operator |
| `builtins_operators_infix` | a reduce step over a user `infix:<>` |
| `builtins_operators_coerce` | a hyper step over a user `infix:<>` |
| `accessors_state` (`call_user_routine_direct`) | `reduce` given the operator as a routine value; the flip-flop infix fallback |
| `main_args` | the selected `MAIN` candidate |

Measured on a `[mm] 1 .. 200` reduce over a two-candidate user
`multi sub infix:<mm>`, release build, pinned to one core: **15.44G -> 13.33G
instructions (-13.7%)** and 1.26s -> 0.98s wall (-22%).

## Two measurement notes worth keeping

**Measure the release build.** The same A/B on the *debug* build says the
opposite — 13.50G -> 15.46G, a 14% *regression*. Debug does not inline the
compiled entry's per-call bookkeeping while `compile_block_raw` stays
comparatively cheap, so the debug numbers invert the real ranking. This is the
wall-clock warning in CLAUDE.md applying to instruction counts too: the
`MUTSU_VM_STATS` *counters* are optimization-independent, a dispatch-path
instruction count is not.

**Do not use `compile_and_call_function_def`.** That general routine entry
pushes a samewith context and a fresh multi-dispatch frame before running the
body. Here the caller has already resolved its candidate, so building a
candidate list per call is pure overhead, and it A/B'd measurably worse than
`call_compiled_function_named` — the entry just below that setup, which also
matches `call_function_def`'s semantics exactly (it pushed neither stack). The
multi-deferral slice avoided the same entry for a stronger, correctness reason
(it re-pushed the frame the deferral chain owns).

While there, `otf_compile_function_def`'s cache key stopped being a SipHash over
the body fingerprint plus a freshly allocated package `String`. Both halves are
already integers — the fingerprint is memoized on the def and the package is an
interned `Symbol` — so they are mixed arithmetically, and the package string is
resolved only on a cache miss.

## What still reaches the interpreter entry, and why

`call_function_def` survives for exactly one gated shape, so C6d-1 is not yet
complete. `multi_candidate_state_forces_interpreter` covers a `state`-bearing
candidate of a name declared with signature *alternates*:

```
multi sub postfix:<CNT> (AltA $x) | (AltB $x) { state $counter = 0; ++$counter }
```

Those alternates are ONE routine with ONE `state` cell, shared through a
compile-time `state_group` that the compiler already threads into every
alternate's compiled body. But `vm_register_sub_ops` attaches plan-compiled
bytecode only to *non-multi* candidates (`if *multi { continue; }`), so a multi
candidate reaches dispatch with `compiled: None` and gets compiled on the fly
under its own signature — one cell per alternate, which
`t/multi-signature-alternates.t` catches. Attaching each compiled routine key to
its multi candidate retires both the gate and the interpreter entry; that is the
next C6d-1 slice.

## Pinned

`t/user-operator-compiled-body.t` (26 assertions, every expectation taken from
`raku` first): the prefix/postfix/infix forms, reduce, triangular reduce, hyper,
cross- and zip-metaops, `reduce(&infix:<...>)`, a two-candidate multi operator
resolving per step, `is rw` writeback, a `state` variable across reduces, a
declared default, a return constraint, a `where`-constrained candidate, a `die`
propagating out, `$?PACKAGE` inside a package-scoped operator, and
`.candidates`. The gated alternates shape cannot be raku-verified — `raku` does
not accept that syntax — so `t/multi-signature-alternates.t` keeps pinning it.
