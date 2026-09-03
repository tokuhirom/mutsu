# The late-August call-path slowdown: profile-driven now, ~20% left on `bench-fib`

Between 2026-08-19 and 2026-08-31 a broad set of call-path-shaped benchmarks got
slower while an unrelated set got faster. Daily medians of the bench-CI series
(`git show origin/bench-data:bench-history.tsv`), 2026-08-17..19 vs
2026-08-31..09-02:

| regressed | | improved | |
| --- | --- | --- | --- |
| `hash-access+jit` | +37% | `bench-array+jit` | −40% |
| `bench-grammar-parse+jit` | +36% | `bench-ctor+jit` | −33% |
| `bench-fib+jit` | +34% | `bench-yaml-parse+jit` | −21% |
| `bench-tak+jit` | +23% | `method-call+jit` | −10% |
| `bench-hash+jit` | +19% | `bench-class+jit` | −9% |

`bench-startup` is flat over the window (+0.4 ms), so this is not startup cost
and is unrelated to `todo/perf/interpreter-new-is-expensive-and-retains-memory.md`.

The largest step was found by bisection and fixed —
`news/2026-09/adr0037-routine-frame-push-intern-cost.md`: #6720 (ADR-0037
Slice 1) added a `RoutineFrame` push to the light call paths whose five per-call
`Symbol::intern`s cost 26% of `bench-fib`. That is now ~3%.

**What remains:** with that fix in place a local release build is still ~25%
slower than a build of `af7c5d6eb4d9` (2026-08-19) on `bench-fib`. Under `perf`
(fib(33), JIT on, pinned to a P-core, `profiling` builds of both) the run costs
**5.504 Gcycles then vs 7.108 Gcycles now, +29.1%**.

## Do NOT keep bisecting — read this first

Bisection found the 26% step cleanly. It does **not** work for what is left, and
a second bisect run proved why rather than producing an answer.

That run named #6784 (`fix/bind-alias-reverse-write`) as the next bad commit, and
an interleaved A/B of its merge against its first parent reproduces a consistent
**+4.7%** on `bench-fib` in both orderings. But #6784 only touches `:=` bind
paths, and `perf` on a build containing it samples **zero** cycles in
`store_through_cell` or `propagate_bind_to_ancestor_frames` while running
`bench-fib` — that code never executes here. The 4.7% is therefore a
**code-layout effect** between two separately-built binaries, not a semantic
regression (the trap is a known one: adding unrelated code moves hot functions).

Since layout noise is itself ~5%, and the remaining regression appears to be
several steps of about that size, per-commit bisection cannot separate signal
from layout. Any commit a further bisect names must be discharged the same way —
check whether its code is even *sampled* in the benchmark — before it is
believed.

## Use the differential profile instead

Symbol-level self-cycle delta, aug19 → today (same script, same box, same
profile). Rust inlining differs between the builds, so treat individual rows as
pointers to a *cluster*, not as exact attributions:

| Δ Mcycles | aug19 | now | symbol |
| ---: | ---: | ---: | --- |
| +509 | 1558 | 2068 | `call_compiled_function_positional_light` (self) |
| +240 | 0 | 240 | `hashbrown::HashMap::get` (outlined; new) |
| +212 | 421 | 633 | `mutsu_jit_1` — **the JIT-compiled fib body itself, +50%** |
| +130 | 874 | 1004 | `exec_call_func_op` (self) |
| +114 | 66 | 180 | `nanbox::payload_op` |
| +107 | 0 | 107 | `unmark_readonly_sym` |
| +90 | 0 | 90 | `current_source_file_sym` — **closed**, see below |
| +88 | 0 | 88 | `replay_readonly_undo` |
| +83 | 0 | 83 | `finish_positional_light_env` |
| +60 | 0 | 60 | `Env::get_sym` (outlined) — **closed**, see below |
| +48 | 0 | 48 | `mark_readonly_sym_with` |
| +36 | 0 | 36 | `decode_arg_slip_positions` |
| −369 | 369 | 0 | `LocalKey::with` — removed by the ADR-0037 intern fix |

The `?FILE` rows are closed by
`news/2026-09/env-carries-its-source-file-symbol.md`: `Env` now carries its own
`?FILE` symbol, so the routine-frame push reads it in O(1) instead of walking
the overlay chain (`hash-access` −5.4%, `fib` −4.9%, `bench-fib` −4.6%,
`bench-ctor` −4.5% locally). `Env::get_sym` and `current_source_file_sym` are
both gone from the top of the profile.

## What is left, from a profile of the current build

`bench-fib` (fib(33), JIT on), self time:

| share | symbol |
| ---: | --- |
| 33.0% | `call_compiled_function_positional_light` (self) |
| 16.8% | `exec_call_func_op` (self) |
| 10.3% | `mutsu_jit_1` |
| 11.3% | `Vec::extend_trusted` + `recycle_locals` + `Vec::resize` — the args/locals pool |
| 5.5% | `replay_readonly_undo` + `HashMap::insert` + `unmark_readonly_sym` — readonly bookkeeping |
| 2.6% | `LocalKey::with` — **closed**, see below |
| 1.8% | `resolve_let_saves_on_success` |

The `Symbol::intern` / `LocalKey::with` rows are closed by
`news/2026-09/hot-path-well-known-symbols-and-param-bind-move.md`: the call
path re-interned the fixed names `"_"` and `"Any"` on every call, and the
parameter bind loop cloned each bound argument twice. `bench-tak` −8.0%,
`bench-fib` −5.2%, `fib` −4.2% locally.

## State after the 2026-09-03 round

Eight PRs landed against this ticket (#7259, #7261, #7262, #7265, #7266,
#7267, #7268, #7269): cumulatively about **−38% on `benchmarks/fib.raku`**
locally. A fresh `bench-fib` profile now reads:

| share | symbol |
| ---: | --- |
| 36.3% | `call_compiled_function_positional_light_at` (self) |
| 14.1% | `exec_call_func_op` (self) — **now ADR-0066**, see below |
| 9.2% | `mutsu_jit_1` |
| 4.3% | `vm_jit::try_enter` |
| 3.2% | `Env::get_sym` |
| ~4.5% | `unmark_readonly_sym` + `replay_readonly_undo` + `HashMap::insert` |

`malloc`/`_int_free` have left the top of the profile entirely (item 6).

**The biggest single remaining item is now `exec_call_func_op`'s two hash
probes per call**, which is the subject of
[ADR-0066](../../docs/adr/0066-call-dispatch-inline-cache.md) (Proposed). Read
that before touching call dispatch: it also records the smaller, no-ADR-needed
first step — putting `CompiledFns` values behind `Arc` — which on its own
removes one of the two probes and makes the other cheap.

## The rest, in rough order of size

1. **The args/locals `Vec` churn (~11%).** The **args half is closed** by
   `news/2026-09/positional-light-call-binds-from-the-stack.md`: the cached
   positional dispatch no longer materializes an argument buffer at all — the
   callee takes `args_base` and binds by move straight out of `self.stack`,
   truncating on every exit path (`bench-tak` −10.6%, `fib` −7.2%, `bench-fib`
   −6.7% locally, both orderings). What remains is the **locals** half: the
   light path still borrows a pooled `Vec` for the callee locals and `resize`s
   it with `Value::NIL` per call, and the same buffer shape is still copied
   into on the *named* light path (`call_compiled_function_light`), which was
   left untouched.
2. **Readonly bookkeeping (~5.5%).** Every call marks each parameter readonly
   and unmarks it on exit: an `FxHashMap<Symbol, ReadonlyKind>` insert, a
   remove, and a journal push/pop per parameter per call. A monomorphic
   recursive call re-marks a symbol its own caller already marked with the same
   kind; the code tries to make that journal nothing but still pays both map
   operations.
3. **`mutsu_jit_1` +50% since August.** The natively-compiled body itself got
   slower, which nothing in the interpreter explains. Dump the generated code
   for both builds before guessing. (Note that `vm_jit_helpers::ret`, the shim
   the generated code calls on every return, was separately fixed — see below —
   so re-measure `mutsu_jit_1` itself before assuming the +50% is still there.)
4. **The remaining `env().get("<fixed name>")` probes.** Closed for `&return`
   by `news/2026-09/jit-return-probes-a-pre-interned-symbol.md`: the rebound-
   `&return` probe on every routine return re-interned the name, a thread-local
   string-keyed hash lookup that was 5.3% of `bench-fib` under `LocalKey::with`
   plus its share of `__memcmp_avx2_movbe` (`bench-fib` −6.6%, `fib` −6.8%,
   `bench-tak` −4.8% locally, both orderings). **75 more `env().get("...")`
   call sites remain** with a literal key — `"_"` (which already has
   `wk::topic()`), `"/"`, `"!"`, `"__mutsu_in_eval"`. None are on `fib`'s path,
   so they need a profile of a different benchmark (loops, smartmatch,
   substitution, `try`) to rank before sweeping.
5. **`call_compiled_function_positional_light_at`'s stack frame.** It was 1064
   bytes and every recursive call touches all of it.
   `news/2026-09/light-call-cold-error-paths-outlined.md` took it to 984 by
   moving the four inline error constructions (`format!` + attribute maps) into
   `#[cold] #[inline(never)]` helpers — `fib` −7.3% cycles / −1.7% retired
   instructions. **984 bytes is still large and nothing else has been
   identified as filling it**; `perf annotate` on the function shows the
   remaining cost as `movaps ...,N(%rsp)` spill stores. Note that the retired-
   instruction count is the honest oracle for this kind of change: the code
   being moved never executes in the benchmark, so a cycles-only measurement
   cannot be told apart from the layout lottery.
6. **`RuntimeError::return_signal` allocated a `String` per return** —
   **closed** by `news/2026-09/runtime-error-message-is-a-cow.md`. `message` is
   now `Cow<'static, str>`, so the control signal every routine return raises
   costs no allocation (and neither does any
   `RuntimeError::new("some literal")`). It was worth far more than the 4-5%
   the malloc/free rows suggested: `fib` −14.8% cycles / **−7.2% retired
   instructions**, `bench-fib` −11.2% / −7.2%, both orderings. `bench-tak`
   barely moved, which is the correct control — its body has no explicit
   `return`.
7. **Three per-call helpers paid a call to discover they had nothing to do** —
   **closed** by `news/2026-09/hot-path-noop-guards-are-now-inlined.md`.
   `apply_pending_rw_writeback`, `apply_pending_caller_var_writeback` and
   `resolve_let_saves_on_success` each begin with a guard that is true in the
   common case, but the guard lived inside an out-of-line function, so the
   common case still paid a call/ret (1.31% + 1.27% + 1.26% of self time).
   Now `#[inline]` wrapper + `#[inline(never)]` body: `fib` −8.7% cycles /
   −5.1% retired instructions. **Look for more of this shape** — a comment
   claiming "no cost when empty" is a hint that the guard is behind a call.

## Method notes for whoever picks this up

- Run solo on an idle box (`uptime`, `pgrep -c -x rustc`).
- **Interleave** the A/B binaries (alternate runs), never measure them in
  sequence — a non-interleaved comparison drifted enough here to invert a 5%
  result.
- This box is a hybrid P/E-core CPU. The default `cycles` event records BOTH
  PMUs, so `perf report` prints one section per event — read the `cpu_core`
  one and ignore the `cpu_atom` section, which typically holds a couple of
  dozen startup samples. To collect only the P-core event, the syntax is
  `-e cpu_core/cycles/u` (the modifier goes AFTER the closing slash;
  `-e cpu_core/cycles:u/` is a syntax error that makes `perf record` fail
  outright — and if an old `.data` file is lying around, `perf report` will
  happily analyze *that* instead, which is how the wrong recipe got recorded
  here in the first place). Pin with `taskset -c 0` either way.
- The `perf` binary is at
  `/usr/lib/linux-tools/6.8.0-138-generic/perf` — not on `PATH`, and its
  version does not match `uname -r`.
- Build both sides with `--profile profiling` (release + debuginfo) for
  symbolized profiles.
- Per `todo/README.md` and CLAUDE.md, any number that ends up in a document
  comes from the bench CI, not from the session's local runs.
- **Measure retired instructions, not just cycles**
  (`perf stat -e cpu_core/instructions/u`). When the code a change touches is
  never executed by the benchmark — an outlined cold path, a shrunk stack
  frame — a cycles delta is indistinguishable from the ~5% layout lottery, and
  it flips sign under a swap exactly the way a real win does. Instruction
  counts are layout-insensitive. Also pick a *control* benchmark the change
  cannot affect and confirm it does not move (`bench-tak` served this role for
  the return-signal fix: no explicit `return` in its body).
- **Run the correctness gate BEFORE the A/B.** A change that accidentally skips
  work measures *faster*: the first A/B of #7269 read `bench-fib` −12.2%
  because the split had captured an unconditional trailing call into the
  guarded slow path. `make test` caught it; the honest number was −8.2%.
