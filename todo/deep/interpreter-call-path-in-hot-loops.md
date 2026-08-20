# The interpreter function-call path is the one place mutsu is slower than raku

Extracted from PLAN.md §5 (2026-08-02). This is the only *measured* perf axis where mutsu loses to
raku on real spec tests, so it is worth keeping even though §5 as a whole is de-prioritized polish.

## The measurement

Roast-wide raku-vs-mutsu wall-clock re-baseline (2026-07-15, `scripts/roast-speed-diff.sh` over the
1358 runnable whitelisted tests, release): mutsu beats raku (Rakudo 2022.12) on ~1348/1358 tests,
usually 2–30× (fast startup + JIT). The **only** files where mutsu is meaningfully *slower* (clean
single-run ratios):

| file | ratio | shape |
| --- | --- | --- |
| `S04-declarations/state.t` | **4.2×** | `for ^2000000 { $ = foo }` |
| `S06-signature/named-parameters.t` | **2.6×** | `for ^1000000 { foo(:color($_)) }` |
| `S07-iterators/range-iterator.t` | 1.7× | |
| `S12-methods/private.t` | 1.6× | |

Isolated micro-repro: a 1M-iteration loop calling a sub is **1.85× slower** than raku positional,
**2.6× slower** with a named argument.

## Root cause

All of these converge on the interpreter function-call path in hot loops. **The JIT bails at the
call boundary** — proven: a positional 1M loop is `MUTSU_JIT=on` 0.74s ≈ `off` 0.72s, i.e. the JIT
does nothing there — so any loop that calls a sub runs the interpreter call path, whose profile is
~15% malloc/free churn per call (frame + named-args structure + `Env::cow_mut` + `Arc::drop_slow`)
plus per-call `current_package` / `Env::get_sym` / param binding (`exec_set_local_op_inner`).

Named arguments add disproportionate cost (mutsu **+46%** vs raku **+6%**): a `String`-keyed
named-args structure is rebuilt on every call.

## Where the work actually is (2026-07-21 update)

The **light positional** path (`call_compiled_function_light_spec`, `vm_call_light_typed.rs`) is
already heavily J4d-tuned — pooled callee locals (`take_locals_from_pool`, no per-call `Vec` alloc),
`std::mem::take` of caller locals, and frame reuse that skips the env clone when the caller's overlay
is still the shared-empty singleton. So the remaining churn is concentrated in the **named / slow**
path (`call_compiled_function_named`: `args.to_vec()` plus the `String`-keyed named-args structure).
Target that specifically: intern the parameter names and bind by `Symbol`/slot.

## Related, already closed

- **Remove SipHash from `compiled_fns`** — done. The table is
  `FxHashMap<Symbol, CompiledFunction>` (`opcode.rs`), so the light-call cache pays neither SipHash
  nor a name `memcmp`. The residual "cache the callee itself to skip the second lookup" idea is
  low-value (FxHash is instruction-neutral per #4976) and would force `compiled_fns` to hold
  `Arc<CompiledFunction>`; do not pursue.
- **Remove the callsite-line marker** — investigated 2026-07-21 and **deferred as not worth the
  churn**. `peek_callsite_line` (`runtime/call_helpers.rs`) scans args on every call, but the
  `__mutsu_test_callsite_line => N` Pair is attached by the parser only to
  `is_test_assertion_callable` names, and those always compile to `ExecCallPairs`, so the scan is
  dead work on the CallFunc/CallFuncNamed light paths. Removing it is still not clean: the
  CallFuncNamed path is *designed* to carry the marker in-band (`expr_call.rs`), and deriving the
  line from the op's ip via `op_lines` breaks line-exactness (the marker captures the *parse-time*
  line, which differs on multi-line assertions — `test-assertion-line-number.t` pins the number).

## Re-measured 2026-08-03, and it is worse than the 1.85× above says

Driving the same shapes with the result *used* (see the trap below), release build, 1M iterations:

| shape | mutsu | raku | ratio |
| --- | --- | --- | --- |
| A `$n = $n + 1` | 143 ms | 53 ms | 2.7× |
| B `$n = outer-fn($n)`, `outer-fn` declared at file scope | 484 ms | 35 ms | **13.8×** |
| C same, but the sub is declared **inside the calling block** | 840 ms | 34 ms | **24.7×** |

Two things stand out, and the second is the new one:

- The per-call cost is ~340 ns (B) on top of a ~140 ns/iteration loop, so a call is worth ~3
  arithmetic iterations. raku's B/C are *faster* than its A because its optimizer inlines the callee
  outright; mutsu's JIT bails at the call boundary (see above), so the gap is the whole call path.
- ~~**C is 1.7× slower per call than B for an identical body and arity.**~~ **Fixed** — see
  `news/2026-08/block-local-sub-call-path.md`. A routine declared inside a block is OTF-compiled and
  was dispatched through `otf_call_cache`, which re-derived the whole callsite analysis on every
  call and moved a ~1 kB `CompiledFunction` in and out of a `HashMap` around each one. Measured by
  retired instructions (`perf stat instructions:u`, 1M iterations, release): C went **9.20 G → 5.70 G**
  against B's 5.54 G, i.e. from **1.66×** B to **1.03×**. The remaining rows (A/B vs raku) are
  unchanged — that is the general call-path cost this ticket still tracks.

Passing the block through a *module* sub (`sub s-amp(&code) { code() }`, imported) adds a further
~1.5× on top of every row — which is exactly the shape every `lives-ok`/`throws-like`/`subtest` body
takes under the vendored `Test`, and is why the real module inflates heavy roast files past the
30 s per-file budget (`todo/deep/vendor-real-test-module.md`).

**Measured negative result (2026-08-03): fixing the block-local surcharge did NOT move the
real-`Test` files.** `MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 mutsu roast/S04-declarations/state.t` is
410.0 G retired instructions before the fix and 409.1 G after (-0.2%). That is expected in hindsight
and worth writing down so nobody re-chases it: `state.t`'s hot loop is
`lives-ok { for ^2000000 { $ = foo } }`, whose callee `foo` is declared at **file scope** — it is
row B plus the module-sub indirection, not row C. The remaining real-`Test` deficit is therefore the
general per-call cost (row B, ~340 ns/call, 13.8× raku) plus the `&code`-through-a-module-sub
multiplier, and *that* is what the vendored-`Test` campaign is blocked on. Attack row B next, not
the declaration site.

### Trap: raku will delete your benchmark

`for ^1000000 { $n = f($n) }` with `$n` never read afterwards measures nothing under raku — its
optimizer removes the loop, and B/C come back at 8–13 ms, i.e. *faster than an empty arithmetic
loop*. That reads as a 140–370× mutsu deficit and is an artifact. Always return the accumulator
from the benchmarked block and print it.

## Prerequisite

The deeper fix (removing per-call env materialization) is the lexical-scope slot campaign —
[docs/lexical-scope-slot-campaign.md](../../docs/lexical-scope-slot-campaign.md) and
[needs-env-sync-blanket-removal.md](needs-env-sync-blanket-removal.md).

## Re-measured 2026-08-14: rows A/B/C are now well-tuned; the real state.t cost lives elsewhere

Re-ran the isolated repros above (release build, 1M iterations, taskset-pinned) after the
intervening perf work (block-local-sub-call-path fix, closure-call setup slimming, mainline-lexical
cell resolution, and others landed since 2026-08-03):

| shape | mutsu | raku | ratio |
| --- | --- | --- | --- |
| A `$n = $n + 1` | ~170-220 ms | ~230-340 ms | mutsu **faster** |
| B `$n = outer-fn($n)`, `outer-fn` declared at file scope | ~560-690 ms | ~230-340 ms | **~2×** |

Row B went from **13.8×** (2026-08-03) to **~2×** — the general named-function-call path this
ticket was originally opened for is no longer the dominant cost. `MUTSU_VM_STATS=1` on row B shows
`jit: compiles=2 entries=1999802 bailouts=0` — the JIT enters the callee body every iteration; the
remaining ~2× is the surrounding call-frame setup/teardown in
`call_compiled_function_positional_light` (arity check, locals-pool take, scoped-overlay env child,
readonly-frame, param bind), which is already the J4d-tuned target described above. No further easy
win was found there in this pass; it is diminishing returns relative to what follows.

**But `roast/S04-declarations/state.t` — the file that motivated the original 4.2×/13.8× numbers —
is still ~8.3-8.5s vs raku's ~0.7s (~12×), because its actual hot loop is a DIFFERENT shape than
rows A/B/C.** The file's slow subtest is:

```raku
lives-ok { sub foo () {$ = 42}; for ^2000000 { $ = foo } },
    'Intensive use of state variable in inline-friendly sub does not hit problems';
```

`foo` is declared *inside* the `lives-ok { ... }` block, and the whole `for` loop runs *inside* that
same block — i.e. inside a block VALUE invoked by a native Rust function
(`test_fn_lives_ok` → `eval_test_callable_body` → `eval_block_value`), not inside ordinary compiled
mainline/routine bytecode. That carrier path (`eval_block_value_inner`) recompiles the block's body
from AST via the full compiler pipeline on every invocation (`compile_block_value_opts`), and
`MUTSU_VM_STATS=1` on this exact shape shows `interpreter_fallbacks=50%` on the inner `foo()` calls
and `jit: compiles=0 entries=0` — the JIT never even attempts this loop. **This is not the
named-function-call-path problem rows A/B/C track — it is a separate mechanism.** Root-caused and
written up in detail, including a first fix attempt that was measured to *regress* this exact file
2.4x and was reverted, in
[eval-block-value-recompiles-every-call.md](eval-block-value-recompiles-every-call.md).

**Update 2026-08-20 — that ticket is retired.** The per-call recompile was fixed by the
`carrier_compile_cache` (`f6a6eb780`), and the `jit: compiles=0` symptom with it; the `state.t`
JIT bailout was a separate, since-fixed ticket. The close-out is
`news/2026-08/eval-block-value-recompiles-every-call.md`. The residual — a `sub` declared inside a
block is OTF-recompiled on every call when the block runs through `call_compiled_closure`, which is
the real mechanism behind that 2.4x regression — is now
[../tickets/nested-sub-in-block-otf-recompiles-per-call.md](../tickets/nested-sub-in-block-otf-recompiles-per-call.md).
That ticket, not this document, is where the state.t-class slowdown should be attacked next.

`S06-signature/named-parameters.t`'s `for ^1000000 { foo(:color($_)) }` shape was not re-isolated in
this pass; row B's improvement plus the still-open named-args churn noted above make it worth a fresh
measurement before assuming its 2.6× still holds.

## Measurement protocol

Confirm with `perf` retired instructions (`instructions:u` + `taskset` core pinning — otherwise it
wobbles 8%) that a candidate actually consumes time. **Reducing opcode count ≠ reducing time**: the
`SetSourceLine` removal was 21% of executed opcodes on fib but only -3.4% instructions (±0 on the
JIT path), and a first implementation that refreshed on every instruction was a +7.8% deficit. See
[ADR-0006](../../docs/adr/0006-baseline-interpreter-optimizations.md) §"Measurement protocol".
