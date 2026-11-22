# ADR-18580: Multi sub and operator calls run a keyed call-site plan

- **Status**: Accepted (2026-11-22, by the maintainer). Conditions: D2 applies only to a winner
  picked by a keyed program, never to a plain call, and its PR carries the `S06-multi` /
  `S12-subset` roast files, `t/routines/dispatch` and an ecosystem parity table.
- **Issue**: [#18580](https://github.com/tokuhirom/mutsu/issues/18580) (`bench-multi-dispatch@section`
  at ~1.75x rakudo; close condition ratio <= 1.00 on that row and its `+jit` row).
- **Builds on**:
  - [#9967](https://github.com/tokuhirom/mutsu/issues/9967) / [#10107](https://github.com/tokuhirom/mutsu/issues/10107):
    the type-keyed `BareMultiPlan` and its `StageProgram` (`src/runtime/multi_dispatch_plan.rs`,
    `src/runtime/multi_dispatch_program.rs`).
  - [ADR-12529](12529-frames-resolve-names-through-the-static-link.md) §2.4: the multi light call
    (`src/vm/vm_multi_plan_light_call.rs`).
  - [ADR-15737](15737-raku-level-call-path-footprint.md) D1: the per-call-site resolved plan.
    D1a (plain method calls) is being built under
    [#18267](https://github.com/tokuhirom/mutsu/issues/18267) (O2 of ADR-18244), and D1b already
    covers multi *methods* and `callsame`/`nextsame` chains. This ADR does not re-decide either.

## 1. Context

`benchmarks/bench-multi-dispatch.raku` has four sections of 30,000 calls each. Local medians of
8 warm runs (release build of main `1d3bd4ae`, 4-core container, raku 2026.11), and callgrind Ir
per iteration with the empty-program baseline (8.4M) removed:

| section | mutsu s | raku s | ratio | Ir / iteration |
|---|---:|---:|---:|---:|
| nominal multi sub (`Int`/`Str`/`Rat`/`Num`) | 0.029 | 0.037 | 0.80 | 10.2k |
| subset / `where` multi sub | 0.082 | 0.053 | 1.54 | 22.8k |
| user `infix:<*>`/`<+>` on `UInt` with `callsame` and `$*modulus` | 0.080 | 0.077 | 1.04 | 23.7k per operator call |
| multi method on a class | 0.093 | 0.054 | 1.73 | 22.7k |

The nominal section already runs on a type-keyed plan and the light call path, and it beats
rakudo. The three losing sections each miss that path for a different reason.

1. **Subset / `where`.** The plan's stage program decides every nominal check once per
   argument-type key (#10107). But a `subset` constraint stays a `ValueCheck::Type { constraint:
   String }`, and every call re-runs `type_matches_value("Small", ..)` by name:
   `resolve_type_in_current_package`, `type_name_binding`, `Symbol::intern`, the base-type check
   and only then the predicate. That is 176M of the section's 685M Ir. Because such a check may run
   arbitrary user code, the program is also not eligible for the multi light call. Each call then
   takes the generic `dispatch_func_call_inner` (505M inclusive), even when the predicate is the
   pure `* < 100` that `SubsetDef::predicate_int_compare` already recognises.
2. **Operator.** A user `multi infix:<*>` is reached through `try_user_infix` →
   `try_user_candidates`. Every call repeats the name-keyed resolution
   (`resolve_function_with_types_keyed`, ~5.0k Ir, of which `run_bare_multi_plan` is 3.7k),
   `core_infix_wins_memoized` (~1.5k) and the proto-body probe (~0.9k). That is about 7.4k of the
   20.7k Ir per operator call, and none of it varies between calls with the same argument types.
   The candidate body (`callsame() mod $*modulus`) is most of the rest.
3. **Multi method.** Multi resolution is ~2.7k Ir per call. The method call frame and bind
   (`call_compiled_method_fast_in_unit`, ~11k Ir for a body of `$n * 2`) is the bulk. That is
   #18267's measured floor (method `$o.m0` 11.2k → ≤3k), not a dispatch cost.

Rakudo's dispatcher specializes a multi call site on the argument types once and keeps only the
value-dependent guards per call. mutsu has the type-keyed half (the plan) but not the site half.
Each call still finds its plan by name, and its value checks are re-resolved by name.

## 2. Decision

### D1. Value checks are resolved when the stage program is built

A stage program is built once per argument-type key. It resolves each value-dependent check to the
cheapest equivalent form then, not per call:

- a `subset` whose predicate is one integer comparison of the value (`predicate_int_compare`), with
  no closure over lexicals and a plain nominal base, becomes `ValueCheck::IntCompare`. Its base type
  is answered by the key, and per call only the comparison runs. `UInt` is the same check
  (`>= 0`). A non-`Int` argument or a user override of the comparison operator falls back to the
  full by-name check, so the verdict is unchanged;
- any other subset keeps `ValueCheck::Type`; a later slice may resolve it to its `Arc<SubsetDef>`
  so the by-name prologue goes, keeping the predicate run as it is.

A check that runs no user code (an inline WhateverCode `where`, an `IntCompare`) is *rerunnable*:
running it twice is unobservable. A program whose checks are all rerunnable is eligible for the
multi light call, as programs with only inline `where`s are today.

### D2. A winner picked by a keyed program binds without re-checking

When the stage program picked the winner, every parameter's nominal type and value constraint has
been verified for these arguments. The light binder then skips those parameters' type checks. It
already skips `where` clauses under the existing `where_verified` flag, and this extends that
flag's meaning from "the `where`s were verified" to "every constraint was verified". This is what
lets a subset-typed parameter use the light bind, which today admits only `FastParamType` names.

This touches the light binder (`src/vm/vm_call_light.rs`, `vm_call_eligibility.rs`), which
#18267 (O2) also changes. Per #18267's split, the D2 change is announced on that issue first, and
whichever PR lands second rebases. It adds a flag, not a new bind path.

### D3. A multi sub or operator call site keeps its resolved plan

The call site of a multi sub or a user operator holds a slot, in the per-interpreter `caches`
subsystem as ADR-15737 D1 requires for thread safety. The slot records the plan it resolved:
`(fn_resolve_gen, family generation, package-search inputs, argument type keys) → Arc<BareMultiPlan>`
and, for an operator, the `core_infix_wins` verdict and the proto-body classification. A call
whose guard matches skips the name lookup, the package search, the plan-key hash and those two
probes. It runs the stage program and calls the winner. A guard miss is the existing path
unchanged (no new slow path: the existing one is the guard-miss tail). The slot shape
(direct-mapped like `call_ic`, or keyed) follows whatever ADR-15737 slice 4 measures and chooses
for D1a, so the two kinds of site slot are one mechanism.

### D4. Multi methods follow ADR-15737 D1b

The multi-method section's remaining cost is the call frame. Its dispatch part becomes D1b of
ADR-15737 once D1a has landed. This ADR adds nothing there beyond D1 and D2, which a multi-method
program also uses.

## 3. Consequences

- Positive: a value-dependent multi with a pure predicate costs about what a nominal one does
  (expected: the subset section from 22.8k toward ~11k Ir per iteration). An operator call drops its
  ~7.4k Ir of per-call resolution. Together with #18267's frame work for the method section, that
  is the margin `@section` needs to reach ratio <= 1.00.
- Positive: no new resolution cache. D1 changes what a program step holds, D3 is a slot pointing
  at the existing plan cache entry, and D2 widens an existing flag.
- Negative: D2 makes the light binder trust the dispatcher. A program that wrongly admits a
  candidate now binds it without the binder's second check, where before the binder would have
  rejected it with a type error. The guard is that D2 applies only to a winner picked by a keyed
  program, never to a plain call. The roast `S06-multi`/`S12-subset` files and `t/routines/dispatch`
  pin it.
- Negative: D3 adds invalidation inputs to a site slot. They are exactly the plan key's own inputs
  plus the generation stamps the plan cache already checks.

## 4. Alternatives considered

- **Cache the winner per (site, value).** Unsound for `where`/subset candidates, and unbounded.
- **Micro-optimize `type_matches_value` by name** (intern once, skip `type_name_binding`). It
  keeps the per-call resolution and the general path, so the subset section stays off the light
  call. The maintainer guidance on #18580 rules out per-op shaving for this row.
- **Fold multi subs into ADR-15737 D1b.** D1b is scoped to method calls and builds on D1a's
  guard. A multi sub or operator has no receiver class to stamp, and its plan already exists
  (#9967), so its site slot is smaller and need not wait for D1a.

## 5. Open questions

- Whether D3's operator slot can also cover `callsame` inside the candidate
  (`dispatch_next_candidate`, ~1.1k Ir): the deferral chain is a property of the plan, so the
  slot could carry it. Decide when D3 is measured.
- The CI runner's ratio (1.75) is higher than this box's whole-benchmark ratio (1.29). Each slice
  quotes the local per-section A/B, and the close condition is read from `bench-history.tsv` only.
