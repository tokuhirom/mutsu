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

## Prerequisite

The deeper fix (removing per-call env materialization) is the lexical-scope slot campaign —
[docs/lexical-scope-slot-campaign.md](../../docs/lexical-scope-slot-campaign.md) and
[needs-env-sync-blanket-removal.md](needs-env-sync-blanket-removal.md).

## Measurement protocol

Confirm with `perf` retired instructions (`instructions:u` + `taskset` core pinning — otherwise it
wobbles 8%) that a candidate actually consumes time. **Reducing opcode count ≠ reducing time**: the
`SetSourceLine` removal was 21% of executed opcodes on fib but only -3.4% instructions (±0 on the
JIT path), and a first implementation that refreshed on every instruction was a +7.8% deficit. See
[ADR-0006](../../docs/adr/0006-baseline-interpreter-optimizations.md) §"Measurement protocol".
