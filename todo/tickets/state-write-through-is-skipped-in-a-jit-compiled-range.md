# A `state` write-through is skipped inside a JIT-compiled range

`news/2026-08/state-vars-belong-to-the-block-clone.md` made a write to a `state`
slot publish straight to the state store (`Interpreter::publish_state_local`), so
a re-entrant call observes a mutation the outer frame has already made — a
`state` is one container shared by every invocation of its clone, but the value
lives in a per-frame slot that otherwise only reaches the store at frame exit.

That publish is emitted by the interpreter's `SetLocal` / `SetLocalDecl` /
increment dispatch arms. The **JIT** lowers `SetLocal` and `SetLocalDecl` to
`vm_jit_helpers::{set_local, set_local_decl}`, which call `exec_set_local_op`
directly and therefore skip it. A whole chunk that declares `state` never
reaches the JIT (`StateVarInit` is outside the Tier A set, so `compile_chunk`
bails), but a hot-loop **range** does when the declaration sits in the enclosing
block and only the write is inside the loop:

```raku
sub f { state $n; for ^BIG { $n = $n + 1; f() if ... } }
```

Such an `f` sees the pre-loop value on re-entry. This is exactly main's
behaviour before the write-through existed, so it is a *residue*, not a
regression — every non-JIT path is now correct.

## Why it was not just fixed

Both obvious fixes cost far too much on the hot path, measured on
`roast/S04-declarations/state.t`, whose test 42 is a 2,000,000-iteration
`sub foo () {$ = 42}; for ^2000000 { $ = foo }` loop (release build, idle box;
main is 8.28s):

| variant | state.t | 2M-loop microbench |
| --- | --- | --- |
| as merged (JIT range skips the publish) | 8.4s | 7.1s |
| publish from the JIT `set_local` shims   | 13.2s | 15.6s |
| bail the JIT range out entirely          | 13.7s | — |

The publish itself is the cost: `scoped_state_key` formats a `String` per call
and `state_vars` is a `HashMap<String, Value>`, so a per-iteration publish in a
natively-compiled loop dominates. The 30s per-file CI budget makes a 1.6x
regression there unaffordable.

## What would actually fix it

Make a state-store access cheap enough that publishing per write is free, then
publish from the shims too:

- key `state_vars` by `(Symbol, u64)` (the key strings are already in the
  constant pool, so the `Symbol` can be interned once at compile time) instead of
  a `format!`ed `String` — this removes the allocation *and* the long-string hash
  from every `StateVarInit`, load, publish and sync;
- or give the state local a shared cell so reads/writes need no store round-trip
  at all, which is the `ContainerRef`-everywhere direction ADR-0001 fuses with GC
  (do NOT start that as a standalone campaign — see the ADR).

The `(Symbol, u64)` rekey is the self-contained one and is worth doing on its own
merits: it also speeds up every ordinary `state` variable.

## Minimal repro

None that fails deterministically today — the shape above needs the loop to be
hot enough to JIT (default threshold) *and* a re-entrant call that reads the
same `state`. Verify a fix with `MUTSU_JIT=off` vs on: the two must agree.
