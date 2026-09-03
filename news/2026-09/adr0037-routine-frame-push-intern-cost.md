# The ADR-0037 routine-frame push cost 26% of `bench-fib`, almost all of it string interning

`todo/perf/bench-hash-ratio-drifted-19-percent-over-late-august.md` recorded
that `bench-hash` had drifted ~18% slower over 2026-08-19 → 2026-08-31 with the
cause unattributed, and proposed bisecting the bench-CI series per commit.

That framing was wrong twice over, and the correction is the interesting part.

## The drift is not about hashes, and not about startup

Comparing every bench-CI series (`bench-history.tsv` on `bench-data`) between
its 2026-08-17..19 and 2026-08-31..09-02 daily medians shows a much broader
picture than one benchmark:

| regressed | | improved | |
| --- | --- | --- | --- |
| `hash-access+jit` | +37% | `bench-array+jit` | −40% |
| `bench-grammar-parse+jit` | +36% | `bench-ctor+jit` | −33% |
| `bench-fib+jit` | +34% | `bench-yaml-parse+jit` | −21% |
| `bench-tak+jit` | +23% | `method-call+jit` | −10% |
| `bench-hash+jit` | +19% | `bench-class+jit` | −9% |

`bench-startup` moved +0.4 ms over the same window, so this is not the
`Interpreter::new()` cost recorded in
`todo/perf/interpreter-new-is-expensive-and-retains-memory.md`. The regressed
column is call-path-shaped — recursion (`fib`, `tak`), grammar rule invocation,
hash-loop bodies — which is what pointed at dispatch rather than at hashing.

The ticket's suggested method also does not work: the per-commit bench-CI rows
for a fast benchmark are bimodal (`bench-fib+jit` alternates between ~0.17 s and
~0.25 s on adjacent commits), so no single row attributes anything. What does
work is a local `git bisect run --first-parent` over release builds, timing
`benchmarks/bench-fib.raku` (the largest, least noisy signal) against a
threshold.

## The first bad commit

That bisect lands on **#6720, "push routine frames on the light sub-call paths
(ADR-0037 Slice 1)"**: `bench-fib` went 0.1167 s → 0.1471 s (+26%) across it,
and its only non-test change is a `push_routine_with_location` / `pop_routine`
pair added to `call_compiled_function_positional_light` and
`call_compiled_function_light[_spec]`.

The frames themselves are correct and must stay — without them a sub taking a
light path ran "frameless", so `enclosing_routine_exists()` answered `false`
inside its body and `EVAL 'return 1'` escaped an enclosing `CATCH`. What cost
26% was how the frame's three `Symbol`s were obtained. Measured on a scaffolded
release build (`bench-fib`, JIT on, median of 11):

| variant | median | delta |
| --- | --- | --- |
| as merged | 0.1526 s | — |
| call-site file `Symbol` skipped | 0.1430 s | −6.3% |
| push/pop removed entirely | 0.1383 s | −9.4% |

`Symbol::intern` hashes the whole string on every call, and the push did it
five times per call:

- `Symbol::intern(&cf.package)` and `cf.source_file.as_deref().map(Symbol::intern)`
  — both constant for the life of a `CompiledFunction`;
- `Symbol::intern(func_name)` — constant for the life of a call site;
- inside `current_source_file_sym()`, `Env::get("?FILE")` interns its *key*
  (`Env::get(&str)` is `get_sym(Symbol::intern(key))`), and then interns the
  returned path string — the longest of the five.

## What changed

- `CompiledFunction` gained `package_sym()` / `source_file_sym()`, lazily
  interned once per compiled function, mirroring the existing
  `param_name_syms` and `CompiledCode::const_sym` precedent. All four
  frame-pushing paths use them.
- The light paths now take a pre-interned `func_name_sym` threaded from the
  call site, which already had it (`CompiledCode::const_sym(name_idx)`, or the
  `name_sym` the by-name paths compute for their dispatch caches).
- `current_source_file_sym()` looks the key up with a process-wide pre-interned
  `"?FILE"` `Symbol`, and memoizes the path intern on the identity of the
  `Arc<String>` the env returns. The env is still consulted on every call, so a
  `?FILE` change is observed immediately; the retained `Arc` is what makes the
  pointer test sound, since it keeps the buffer from being freed and a
  different string landing at the same address.

Local interleaved release A/B against `main` (median of 15 runs each,
alternating binaries on an otherwise idle box): `fib` −12.4%, `bench-fib`
−9.8%, `hash-access` −8.5%, `bench-tak` −5.5%, `bench-hash` −5.3%, with
`method-call`, `poly-call`, `bench-ctor` and `bench-startup` neutral. The
authoritative figures are whatever the bench CI records for the merge commit.
The residual cost of the frame push itself — the `Vec` push, the `pop`, and the
`next_invocation_id()` atomic — is now about 3% of `bench-fib`, down from ~26%.

## What is still open

This closes the ADR-0037 step, not the whole late-August drift: after the fix
`bench-fib` is still ~24% slower than the 2026-08-19 build. That remainder is
tracked in `todo/perf/late-august-call-path-slowdown-remainder.md`, which the
old `bench-hash` ticket was renamed and rewritten into.
