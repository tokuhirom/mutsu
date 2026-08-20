# `WheneverScope` discards its analysis CompiledCode, so a closure created in a `whenever` body loses cross-thread writes

**Status: re-triaged 2026-08-20 against `main` (`9963ef696`). Downgraded from
`todo/deep/` to `todo/tickets/` — this is NOT a dual-store/architectural problem.
It is one unwired opcode field. Ready for direct implementation.**

This file was originally filed as
`todo/deep/cross-thread-captured-lexical-writes-invisible-until-teardown.md`,
framed as a general "cross-thread captured lexical writes are invisible until
scope teardown" defect with `env_dirty`-shaped blast radius. Re-verification
falsified both halves of that framing. The real defect is narrower, sharper, and
*worse* than described, and the fix is mechanical.

## What the original filing got wrong

1. **It is not general to cross-thread captured lexicals.** Every ordinary
   nesting already works: a `start` in mainline, in a `map`/`for`/`if`/`while`
   body, inside another `start`, three `start`s deep, inside a `supply` block,
   inside a named sub called from a `whenever`, and a closure stored in a
   variable or passed as an argument and invoked on a worker. All verified
   correct against `raku` on `main`.
2. **The writes are not merely stale — they are lost.** The original text says
   "The writes are not lost -- they appear all at once once the enclosing scope
   tears down". For the core shape they never appear at all, including after
   `await` of the very promise that performed the write:

   ```raku
   my $x = 0;
   my $p;
   react { whenever Promise.in(0.03) { $p = start { $x++ } } }
   await $p;
   say $x;   # raku: 1     mutsu: 0
   ```

   Instrumenting the worker shows it runs and observes `$x == 1` *inside* the
   `start`; the mainline binding simply never sees it. The staleness the
   original repro showed was a downstream symptom of the same lost write.

## Root cause

The `whenever` body is not compiled as a nested closure of its enclosing frame.
`compiler/stmt.rs` stashes it as a raw `Stmt::Block` in the `stmt_pool`
(`src/compiler/stmt.rs:4117`) and it is re-compiled at runtime by a fresh
`Compiler::new()` (`src/vm/vm_dispatch_helpers.rs:433-438`), where an enclosing
lexical is a *free variable with no local slot* rather than a local. So the
inner `start`'s `box_captured_lexicals` bails at
`src/vm/vm_register_ops.rs:915-918` (`code.captured_mutated_locals` has no such
local) and again at `:989-991` (`resolve_capture_slot` finds no slot), and the
closure captures a plain by-value snapshot instead of a shared `ContainerRef`.
The name-keyed `shared_vars` lane is *also* switched off for that name, because
`block_captured_scalars` (`src/runtime/runtime_thread.rs:57-79`) sees a plain
captured scalar and assumes the closure machinery owns it. Both lanes off = a
silently private copy.

The compiler already anticipates exactly this. `Stmt::Whenever` builds an
analysis-only compile of the body precisely so the enclosing frame can box the
lexicals it captures — `src/compiler/stmt.rs:4126-4151`, whose comment reads
"Case B (cross-thread lexicals)", calling
`surface_stashed_body_free_vars` (`src/compiler/helpers_sub_body.rs:792-820`).
That helper's own doc comment says it "Returns the analysis closure's index into
`closure_compiled_codes` so the emitting op can hand it to
`box_captured_lexicals` at runtime", and the index is duly carried on the opcode
as `OpCode::WheneverScope { analysis_cc_idx, .. }`.

**The dispatch then throws it away.** `src/vm/vm_exec_dispatch.rs:4798-4813`
destructures it as `analysis_cc_idx: _`, and `exec_whenever_scope_op`
(`src/vm/vm_scope_ops.rs:167`) does not even take the parameter. The analysis is
computed on every compile and never used.

`gather` — the other stmt_pool-stashed body, with the identical problem shape —
does it correctly: `MakeGather` resolves its analysis cc and calls
`box_captured_lexicals` (`src/vm/vm_register_ops.rs:106-107`). That asymmetry is
directly observable:

```raku
my $t = 0; my @pt;
my @g = gather { for ^2 { @pt.push(start { $t++ }); take 1; } };
@g.eager; await @pt;
say $t;    # raku: 2   mutsu: 2   -- gather passes its analysis cc

my $u = 0; my $pu;
react { whenever Promise.in(0.03) { $pu = start { $u++ } } }
await $pu;
say $u;    # raku: 1   mutsu: 0   -- whenever discards it
```

Confirming the diagnosis from the other side: forcing the cell to exist by any
other means makes the whenever case correct. Adding a mainline escaping closure
over the same variable (`my $esc = sub { $x++ };`) before the `react` boxes the
slot, and the identical `react`/`whenever`/`start` then reports `1`. The only
missing ingredient is the boxing call.

## Fix

Mirror `MakeGather`. Pass `analysis_cc_idx` through the dispatch into
`exec_whenever_scope_op`, and before `run_whenever_with_value` clones the env
(`src/runtime/subtest.rs:419-428`, `self.env.clone()`) do:

```rust
let analysis_cc = Self::resolve_closure_code(code, analysis_cc_idx);
self.box_captured_lexicals(code, &analysis_cc);
```

That boxes the enclosing frame's slot+env entry first, so the whenever
callback's env clone carries the live cell, the inner `start`'s
`capture_closure_env` picks up a `ContainerRef`, and
`block_captured_scalars`' `ContainerRef` arm
(`src/runtime/runtime_thread.rs:73`) becomes true rather than vacuous.

Note the bubbling half of the machinery is already in place and does not need
touching: `needs_cell_free_vars` (`src/opcode.rs:3927-3937`, folded at
`:5834-5844`) propagates a nested escaping closure's cell requirement up through
intermediate non-escaping frames, and `surface_stashed_body_free_vars` registers
the analysis cc with `escapes = true` via `add_closure_code_baked(analysis,
true)`. Only the runtime call site is missing.

## Affected files

- `src/vm/vm_exec_dispatch.rs:4798-4813` — the discarded field.
- `src/vm/vm_scope_ops.rs:167-265` — `exec_whenever_scope_op`, needs the param
  and the boxing call.
- `src/vm/vm_register_ops.rs:106-107` — the `MakeGather` precedent to copy.
- `src/compiler/stmt.rs:4126-4151`, `src/compiler/helpers_sub_body.rs:792-820` —
  the analysis cc that already exists.

## Suggested pins

A new `t/whenever-body-closure-cross-thread-cell.t` covering: `start` created
directly in a `whenever` body; the same nested one level deeper (inside a `map`
or `for` within the body); an `@`-element write; and the `gather` twin as a
non-regression. The matrix above (mainline / `map` / `for` / `if` / `while` /
`supply` / nested `start` / named sub) all already pass and are worth keeping as
negative controls so a future change cannot "fix" whenever by regressing them.

Watch `roast/S17-*` and the existing supply/react pins for the perf gate:
boxing is now triggered once per `WheneverScope` execution, so a `whenever`
re-tapped in a hot loop pays an extra `box_captured_lexicals` scan. The
early-out at `src/vm/vm_register_ops.rs:908-914` should make that ~free when the
enclosing frame has no `needs_cell_locals`, but confirm with `MUTSU_VM_STATS`
rather than assuming.

## Related decisions

- **ADR-0025** (captured scalar cells value-kind-blind). Its slice 2 was closed
  out on 2026-08-20 with the premise that "what remains classified non-escaping
  (control-flow bodies, sort/map/grep predicate blocks) is correctly
  non-escaping: those blocks are invoked synchronously and never stored, so no
  staleness window exists for them." That conclusion is correct for the shapes
  it examined — `map`/`for`/`if`/`while` all verify green here — and this
  ticket does not reopen it. The `whenever` body is not a counter-example to
  the escape *verdict*; it is a body the verdict machinery never gets to see,
  because the analysis it produces is discarded at dispatch.
- **ADR-0010** (cross-thread lexical sharing scoped to a spawn lineage) and
  **ADR-0027** (loop-frozen value captures cascade through nested closure
  creation) describe the two lanes involved. Neither needs amending.

## The live test this is costing

`t/react-nested-whenever-on-demand-close.t` subtest 1 is this shape. Its
`done if $closed` never fires, so the react runs to its 5-second
`whenever Promise.in(5)` backstop and the interval ticks ~250 times before the
final `ok $closed` passes on the post-teardown value. Re-measured 2026-08-20:
the file takes **5.1 s**, essentially all backstop, making it one of the slowest
files in `make test`. Its own comment says the backstop exists so that "a
genuine regression (closing never fires) fails cleanly with closed=0 rather than
hanging" — in practice it is absorbing this defect instead.

When the fix lands, subtest 1 should complete in ~0.04 s like the synchronous
subtest 2, and the file is worth tightening to bound the observed tick count so
the backstop cannot silently absorb a regression again.
