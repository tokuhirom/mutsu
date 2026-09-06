# A method builds its implicit `*%_` only when its body can observe it

`todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md` had, for the first time, a *specified*
next step rather than a speculative one. The `alloc_scope!` accounting landed in the previous
session named the largest single remaining item on `bench-ctor`'s construction path:
`mfast:slurpy-captures-locals`, at 10.3 allocations per compiled method call — about 10% of the
whole program — and essentially all of it in `implicit_method_named_slurpy`.

## What was happening

Every Raku method carries an implicit `*%_`: a Hash of the named arguments no explicit named
parameter consumed. It exists so a body can write `self.bless(|%_)` without declaring anything,
and so `%_` reads as an empty Hash rather than `Any` (which would splat a stray positional).

The compiled method fast path materialized it on *every* call: a `HashMap` grown incrementally, a
`String` per leftover named key, and a `Value` hash — whether or not the body could ever look at
it. `bench-ctor`'s `submethod TWEAK(:$!spec) { }` has an empty body and built a 7-key hash on each
of its 5000 calls, purely to drop it.

## The gate

`CompiledCode` now carries `may_observe_named_slurpy`, computed while the body is compiled, in the
shape of the existing `uses_dispatcher` flag (which exists for exactly this reason: so a method
that never defers pays no per-call `SamewithContext` clone). The fast path builds `%_` only when it
is set.

The flag is deliberately an over-approximation — a false positive merely keeps the old behaviour,
so the analysis only has to be *complete*, not tight:

- **Any string constant that spells `%_`.** Every way a body reaches the slurpy — a read, a `|%_`
  flatten, an index, a store, an explicitly declared `*%_` used in the body — compiles to an opcode
  whose name operand is the variable-name string `%_` in the constant pool, so one substring test
  in `add_constant` catches the lot. A source literal that merely contains `%_` trips it too.
- **A nested closure whose own flag is set.** A closure in a method body resolves `%_` through the
  method's env, so `add_closure_code` folds the child's flag into the parent's.
- **The dynamic escape hatches**, checked in `emit()`: `EVAL`/`EVALFILE`, symbolic deref
  (`::('%_')`), the `CALLER::` pseudo-package ops, and any inner routine/subset declaration — whose
  body lives in `decl_plans`, not in the constants `add_constant` scans.

Rakudo, checked as the oracle, actually rejects both `EVAL '%_...'` ("Cannot use placeholder
parameter %_ outside of a sub or block") and `%_` in a nested `sub`'s body ("Placeholder variable
'%_' cannot override existing signature"), so two of those hatches guard against paths that are not
reachable in correct Raku at all. They cost nothing and stay.

## Measurements

Allocation counts (`--features alloc-stats`, exact and load-independent), `benchmarks/bench-ctor.raku`:

| scope | before | after |
| --- | --- | --- |
| `mfast:slurpy-captures-locals` per method call | 10.3 | 4.0 |
| whole program | 1,541,606 | 1,444,372 (-6.3%) |

Order-swapped min-of-11 and min-of-13 wall-clock A/B, `MUTSU_JIT=off`, both binaries built from the
same tree:

| benchmark | round 1 (new first / base first) | round 2 (new first / base first) |
| --- | --- | --- |
| `bench-ctor` | -1.2% / -6.0% | -6.0% / -5.2% |
| `bench-class` | -2.0% / -6.7% | -7.1% / -5.9% |

Faster in every order in both rounds. `method-call`, `poly-call`, `bench-grammar-parse` and
`bench-yaml-parse` were checked for a regression and showed none — their deltas flip sign with
measurement order, which is what noise looks like on a sub-150ms benchmark.

`t/implicit-named-slurpy-gate.t` pins the semantics each branch of the gate has to preserve, and
passes under rakudo as well as mutsu.
