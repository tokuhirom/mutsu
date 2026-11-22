# ADR-8673: TRIR-to-TRIR calls push no routine frame until something can observe it, and small callees are inlined

- **Status**: Proposed (2026-11-22)
- **Issue**: [#8673](https://github.com/tokuhirom/mutsu/issues/8673) (goal:
  `bench-json-fast-spdx@section+jit` below 1.0x rakudo; 1.65-1.71 on 2026-11-21).
- **Builds on**: [ADR-0110](0110-typed-resolved-ir-for-statically-typed-routines.md) (TRIR),
  [ADR-0112](0112-trir-completion-plan-for-beating-rakudo.md) (its Step 1 run-time linkage),
  [ADR-0116](0116-trir-native-lowering-measured-before-building.md).
- **Reopens**: ADR-0116 §6's rejection of "inline small callees first". That rejection rested on
  §3.3's measurement that TRIR frames and calls were 3.5% of a record. Re-measured below, they are
  18%.

## 1. Context

### 1.1 Where a decode goes now

Callgrind, `--profile profiling`, the benchmark's 100-record document decoded once and twice in
one process (startup, module load and document building cancel out), on `main` plus #18599 (the
packed `Uni` store): **62.1 M instructions per decode**, ~620 k per record.

| where | share |
| --- | ---: |
| the TRIR switch loop (`run_trir_chunk` self, with the arms inlined into it) | 30% |
| TRIR-to-TRIR call machinery | 18% |
| NaN-box decode, refcount and drop | 9% |
| the allocator | 8% |
| everything else (op bodies: `ordat`, `eqat`, `findnotcclass`, `index`, `bindkey`, the `Uni` store, ...) | 34% |

The decode runs entirely in TRIR: `MUTSU_VM_STATS` counts no bytecode op during it, so the
bytecode VM's narrow ops (ADR-18357) do not reach this row.

### 1.2 What one call costs

A module with `my sub f(str $t, int $pos is rw --> Nil)` of 5 ops, called from a TRIR loop,
against the same loop with `f`'s body written inline: **~1,250 instructions per call**, of which
~200 is `f`'s own body. The ~1,050 of overhead:

| part | Ir |
| --- | ---: |
| `exec_trir_inner_call` itself: guards, argument copy, frame setup | ~325 |
| the routine-stack entry: `push_routine_with_visibility` (with `current_source_line`, `executing_source_file_sym`, `lexical_package_for_frame`), `RoutineStack::pop`, the `current_unit` swap | ~300 |
| operand-bank truncation and frame pop (`truncate`, `pop_frame`, `resize`, `extend_with`) | ~220 |
| `run_trir_routine`, `trir_seed_outers`, `trir_reseed_outers`, result push, fatal-arg check | ~200 |

A JSON::Fast record makes ~85 such calls: `nom-ws` four or five times per object key, plus
`parse-thing`, `parse-string`, `parse-string-slow`, `unjsonify-string`, `parse-true` and their
kin. Most of them are small: `nom-ws` is 13 ops, `parse-true` 6.

### 1.3 Who reads the routine stack from inside a TRIR chunk

The routine-stack entry is the expensive part that carries semantics, so the question is who can
observe it. A survey of every reader (2026-11-22) found:

- **Calls out of TRIR** (a `CallGen` that is not linked, a method callout): the callee sees the
  stack through `callframe`, `CALLER::`, backtraces and `&?ROUTINE`. So does user code that an op
  runs without a call op: `.Bool` from `TruthyObj`, `.defined` from `TruthyDefined`, `.Str` from
  the `Concat*` ops, a subset's `where` predicate during a type check.
- **`nqp::die`, `nqp::throw`, `nqp::rethrow`**: they snapshot the stack as the backtrace at the
  throw (`attach_lazy_backtrace_to_error`).
- **Warnings** raised by an op (a type object stringified by `Concat*`): `write_warn_to_stderr`
  renders a backtrace, and a `CONTROL` handler can run user code.
- **Type-name resolution on its miss path** (`running_package_candidate_syms`, from
  `type_matches_value` and `ClassOperand`) and **`CallGen` arming** (`trir_gen_context`,
  `plain_fn_resolve_key`): these read only the top frame's `package`, `lexical_package` and
  `def_file`.
- **`frame_invocant`** (from `nqp::iscont*` on an attribute): reads the top frame's fixtures.
  Without the entry it would see the caller's invocant.

Everything else (`fail_error_to_failure_value`, `$*` lookup, `RuntimeError::new`, the return-type
check, parameter binding) does not read it. An error an op body raises without `die` already gets
its backtrace late, from the untyped op that called into TRIR, after the TRIR frames are popped:
backtraces through TRIR frames already omit them, and this ADR does not change that.

## 2. Decision

### D1. The routine-stack entry of a TRIR-to-TRIR call is pushed lazily

A TRIR-to-TRIR call (`CallTr`, and `CallGen` served through its link) records what the entry needs
on the TRIR frame it pushes anyway: the callee chunk (name, `def_file`, hidden-from-backtrace),
the call-site line and file (computed eagerly, because `executing_source_file_sym` reads the stack
itself), and the package. It does not push a `RoutineFrame` and does not swap `current_unit`.

`TrStacks` keeps one index: how many of its frames have their entry on the routine stack. Before an
op that can observe the stack (§1.3) runs, `trir_materialize_frames` pushes the entries of every
TRIR frame above that index, bottom up, and swaps `current_unit` for the innermost one. A frame's
return pops its entry only if it was materialized.

Which op observes is a static property of the op, decided by the compiler and stored per chunk as
a bit per op, not re-derived at run time:

- observing: `CallGen` (linked or not; the link check itself reads the top frame), method
  callouts, `NqpOpGen` of `die` / `throw` / `rethrow` and any generic `nqp::` op not in the
  pure list, `TruthyObj`, `TruthyDefined`, `Concat*` on a non-`Str` operand class, every op with
  a type check against a non-builtin type, `ClassOperand`;
- pure: native int/num arithmetic and compares, jumps, slot loads and stores, `ordat`, `eqat`,
  `findnotcclass`, `index`, `substr` on `str` slots, the typed list ops (`ShiftI*`, `PushI*`,
  `Elems*`, `AtPosI`), `bindkey` / `p6scalarwithvalue` on proved storage.

A `CallTr` to a chunk that itself contains no observing op never materializes anything; a chunk
whose only observing op is on a cold branch (`nom-ws` reaching `nom-comment` only on `/`)
materializes only when that branch runs.

### D2. Small non-recursive callees are inlined into the caller's chunk

At the point a call site is linked (statically for `CallTr`, at first observation for `CallGen`),
a callee under an op budget (initially 24 ops) that is not recursive is spliced into the caller's
op stream: its slots renumbered into the caller's frame, its `is rw` native parameters bound to the
caller's slots, its `Return*` turned into a jump to the join. The inlined region keeps a record
(callee chunk, call-site line) so that D1's materialization can push the entry the callee would
have had, exactly as it does for a real TRIR frame. So an inlined `nom-ws` that reaches its cold
`nom-comment` call still shows `nom-ws` in the backtrace.

A `CallGen` site is linked at run time (ADR-0112 Step 1), so its inlining is a re-link: the site
keeps pointing at the caller's chunk, and the caller is recompiled with the callee spliced in when
the link is first established and stays current (`link.current_in`). A link that later goes stale
falls back to the call exactly as today.

### D3. Order and gates

| stage | content | gate |
| --- | --- | --- |
| 1 | D1, with a per-op observing bit and `trir_materialize_frames`; `MUTSU_TRIR_LAZY_FRAMES=0` restores the eager push for bisection | the §1.2 microbenchmark's overhead ≤ 750 Ir per call; `t/vm/codegen/adr0110-trir-differential.t` and a new backtrace-through-TRIR test identical with the switch on and off |
| 2 | D2 for `CallTr` callees | the 100-record decode ≤ 52 M Ir (from 62.1 M); backtraces from an inlined callee's cold path identical to stage 1 |
| 3 | D2 for linked `CallGen` callees | measured on its own |

**Expected gain.** Stage 1 removes ~300 of ~1,050 per call, ~4% of a decode. Stage 2 removes the
whole call for the inlined callees, most of the 85 calls per record: ~15% of a decode. Together
with #18599 (-12.5%) that is ~1.35-1.4x of the 1.65-1.71x the row needs. The rest is the switch
loop and the op bodies, which is ADR-0116 D3's native lowering; inlining is also what that
lowering needs first (ADR-0116 §6 named callee inlining as the missing half of the prototype).
This ADR does not claim to close the row.

## 3. Alternatives considered

- **Make the eager push cheaper.** The push already avoids most of its old cost (#8811 and the
  `is_qualified` memo). Its remaining parts read the source position and the lexical package, and
  each is needed by some reader. Making it cheaper keeps paying it on every call; not pushing pays
  nothing on the ~all calls nothing observes.
- **A per-chunk "never observes" flag instead of per-op materialization.** Simpler, but `nom-ws`,
  the most frequent callee, has an observing op on a cold branch and would be excluded.
- **Lean frames for every call kind** (ADR-18244 O2). That covers the bytecode VM's calls and is a
  much larger change; TRIR's calls are already a separate, smaller path, and this ADR does not
  touch the VM's.
- **Native lowering first** (ADR-0116 D3). Its condition (the switch loop above 50% of a record)
  is not met (30%), and its prototype lacked callee inlining.

## 4. Consequences

- The routine stack seen from inside TRIR is correct at every point something can look at it,
  and stale in between. Every new TRIR op has to be classified as observing or pure; an op
  classified pure that can in fact run user code or throw with a backtrace is a wrong backtrace
  or a wrong `callframe`. The bisection switch and the differential test are what catch it, and
  the default for an unclassified op is observing.
- `frame_invocant` stays correct because `nqp::iscont*` is an observing generic op.
- Inlined code is attributed to the callee in backtraces, through D1's region record.

## 5. Open questions

- Whether `current_unit` must be swapped for a pure op at all (no surveyed pure op reads it).
- The inlining budget, decided by stage 2's measurement.
