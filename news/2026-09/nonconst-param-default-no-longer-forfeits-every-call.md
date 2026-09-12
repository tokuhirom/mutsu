# A non-constant parameter default no longer forfeits the light path on calls that supply everything

`sub f($x, $y = $x + 1)` was 6.6x slower per call than `sub f($x, $y = 2)` —
**including on `f(1, 2)`, which never consults the default at all**. The routine
is now served by the positional light-call path whenever the caller supplies
every positional, which closes that gap completely: 744.5M instructions for a
20 000-iteration loop of full-arity calls becomes 139.8M, against 139.6M for the
same loop with a constant default.

## What was happening

`CompiledFunction::light_required_positionals` answers `None` for a signature
with an optional parameter that `const_fill_for_param` cannot reduce to a shared
constant — a default reading an earlier parameter (`$y = $x + 1`), an arbitrary
expression (`$y = expensive()`), a container literal (`$y = []`, which must be a
*fresh* container per call). `is_positional_light_call_eligible` required it to
be `Some`, so the whole routine stayed on the general binder.

That verdict is entirely about what the light bind would have to **produce for
an omitted parameter**. It says nothing about a call that omits nothing — and
those calls paid it anyway: a full by-name `resolve_function_with_types` and a
full `bind_function_args_values` on every one. Measured with `MUTSU_VM_STATS=1`,
2 000 calls of `f(1, 2)` cost 2 001 by-name resolves; the same loop against a
constant default cost 1.

## The fix

A new `CompiledFunction::light_full_arity_only`, set where
`light_required_positionals` is computed, marks exactly that signature. Every
dispatch site pairs it with an exact arity test against `param_local_slots`
(`Interpreter::positional_light_full_arity_call`); a call short of full arity
still falls through to the general binder, which remains the only thing that can
evaluate the default.

The per-call re-check matters because `pos_light_call_cache` is keyed by
**name**: one entry serves every arity the call sites use, so a routine admitted
by `f(1, 2)` would otherwise be served at `f(1)` too, where the light bind has
no value for the omitted parameter and would raise "Too few positionals" for a
call the general binder defaults happily. `t/routines/signature/nonconst-param-default-full-arity-light.t`
alternates the two arities against one routine to pin that.

The flag is set only where the precompute actually ran, so a hand-built chunk —
whose `light_required_positionals` is `None` because nothing computed it — is
not admitted by it.

## The admission refuses a call carrying a `Pair`, and that is not incidental

The light bind takes every argument positionally, so it binds a named argument
to a positional parameter where rakudo rejects the call: `sub f($x, $y = 2) { };
f(1, :verbose)` answers `1/verbose` in mutsu and "Unexpected named argument" in
rakudo. That bug predates this work and is filed separately as
[#8077](https://github.com/tokuhirom/mutsu/issues/8077) — the fix needs a
per-callsite "has a named argument" bit, because at bind time `f(1, :verbose)`
and a legitimate positional `Pair` (`f(1, (y => 2))`) are the same `Value`.

What matters here is that the admission must not *widen* it. A routine with a
non-constant default has always reached the general binder, which rejects the
call correctly, and an unguarded admission changed that answer — caught by
running the case against both binaries rather than by any test. So
`positional_light_full_arity_call` refuses any `Pair`-carrying call, which
reproduces those routines' behaviour exactly; the cost is that
`f(1, (y => 2))` merely forgoes the speedup. The guard can be dropped once
#8077 is fixed.

## Cost on the hot path: none measurable

The guard is last in the cached dispatch's condition chain and behind the flag,
so an ordinary routine pays one bool test: neither the `param_local_slots` load,
the callsite-line marker peek, nor the `Pair` scan is reached for it.

Callgrind instruction counts, release, `MUTSU_JIT=off MUTSU_GC=off`:

| | before | after | |
| --- | --- | --- | --- |
| 20 000 full-arity calls, non-constant default | 744 296 141 | 140 928 064 | **-81.1%** |
| 20 000 full-arity calls, constant default | 139 554 038 | 139 500 624 | -0.04% |
| `fib(22)` | 224 434 376 | 224 324 859 | -0.05% |

The first row lands on the second: the forfeit is gone, not merely reduced.

**On the whole-program benchmarks, the noise is larger than the effect, so no
delta is quoted.** Three callgrind runs of the *unmodified* binary on
`bench-ctor.raku` gave 1 415 573 316, 1 411 120 289 and 1 424 610 493 — a 0.95%
spread — so that program is not instruction-deterministic here and a sub-1%
delta on it means nothing. The measured after/before pairs (`bench-ctor`
-0.35%, `bench-class` +0.05%) sit inside that spread and inside the +/-0.8%
codegen-drift band #7964's note documents for this area. The two probes above
*are* deterministic (three runs of the baseline spanned 1 800 instructions on
`fib(22)`, 0.0008%), which is why they carry the claim.

Shape matters here at a level comparable to the whole effect, so two rejected
shapes are worth recording. Hoisting the marker peek out of the branch — the
obvious way to avoid testing it twice — put a `cl.is_some()` test on every
cached light call and measured +0.317% on `bench-ctor` against +0.115% for the
lazy form; the peek is therefore repeated inside the branch on purpose. And the
guard is deliberately last in the `&&` chain, so the flag's load is the only
thing an ordinary call pays.

## What was measured and deliberately NOT built

[#7581](https://github.com/tokuhirom/mutsu/issues/7581) also asked for tier 3 of
the original plan: compiling each default expression into the callee's own
prologue, so that an **omitted** parameter runs the default's bytecode instead
of `eval_param_default` cloning the AST and compiling it afresh per call. The
issue told whoever took it to count the remaining tail first and close the ticket
rather than build the compiler change if it was thin. It is thin.

Over the whole `t/` suite (4 062 files), with `MUTSU_VM_STATS=1`:

| | count |
| --- | --- |
| parameter defaults **evaluated** (`eval_param_default`) | 140 |
| parameter defaults filled from the constant table | 56 959 |
| by-name function resolves | 251 330 |
| calls the light path refused *only* because of a default | 146 |

47 files evaluate a default at all, at most 13 times each; 46 lose a light call
to one, at most 10 times each. Across every benchmark in `benchmarks/` both
counts are **zero**. So the per-call recompile the issue called "the larger of
the two" costs the entire test suite 140 evaluations, and a per-parameter entry
point in the callee prologue — plus the JIT's view of it — would buy nothing
measurable. That half stays unbuilt; the forfeit that ran on *every* call, which
the original ticket did not separate out, is what was worth fixing.
