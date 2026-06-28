# Seq single-pass consumption design

> **Status:** DRAFT (2026-06-28). This document captures the analysis behind
> the `X::Seq::Consumed` / `+foo` vs `+@foo` track called out from
> `TODO_roast/BLOCKERS.md`. It is intentionally separate from
> [lazy-arrays.md](lazy-arrays.md): the core problem here is **consumption
> identity**, not infinite reify-on-demand.

## 0. Scope

This document is about the semantics of a Seq that is **not cached**:

- first iteration consumes it
- second iteration must throw `X::Seq::Consumed`
- `+foo` must preserve a single Seq as Seq
- `+@foo` must force array/list context and produce a List

This document is **not** about:

- preserving infinite sources when assigned into `@`
- lazy array mutation/reify-on-write
- `X::Cannot::Lazy`

Those belong to [lazy-arrays.md](lazy-arrays.md).

## 1. Why this needs its own track

`roast/S06-signature/slurpy-params.t` still has two unrelated halves:

1. `oneargraw(1..*)` style cases: true lazy slurpy
2. single-pass Seq identity and re-consumption: `+foo`, `+@foo`, second iteration

The first half is "how does an infinite source stay lazy?".
The second half is "who owns the consumed bit, and which APIs observe it?".

Trying to solve both in one campaign causes analysis drift. We need a separate
design for the second half.

## 2. Current behavior

There is already a partial substrate.

- `src/runtime/methods_dispatch_match2.rs`
  - `.iterator` on `Value::Seq` checks `seq_is_consumed(items)` and throws
    `seq_consumed_error()` when the Seq is not cached.
  - if the Seq is not cached, `.iterator` marks it consumed via `seq_consume(items)`.
- `src/runtime/methods_call_dispatch.rs`
  - selected methods (`iterator`, `list`, `List`, `eager`, `Array`, `flat`,
    `Slip`, `join`, `is-lazy`) already guard on consumed non-cached Seq.
- `src/runtime/test_functions/comparison.rs`
  - test helpers already know how to surface `X::Seq::Consumed`.

So the problem is not "no support exists". The problem is that the support is
**scattered and incomplete**.

## 3. Root cause

Today mutsu still mixes two models:

1. **Seq as a materialized list**
   - many call paths happily turn Seq into `Vec<Value>` / `Array`
   - once that happens, consumption identity is lost
2. **Seq as a single-pass iterator source**
   - some paths correctly call `.iterator` and honor consumed state

As a result:

- one path throws `X::Seq::Consumed`
- another path silently re-materializes
- another path preserves the wrong outer type (`+foo` becomes List/Array too early)

The missing abstraction is:

> a clear distinction between **Seq-preserving read** and **list-forcing read**

## 4. Semantic target

We should normalize around these rules.

### 4.1 Seq-preserving operations

These keep a Seq as Seq and must preserve its single-pass identity:

- `+foo` (sigilless single-argument-rule slurpy capture)
- plain variable pass-through where the callee expects "one positional value"
- methods that conceptually operate on the Seq object itself, not its fully
  materialized values

### 4.2 List-forcing operations

These intentionally force list/array context and therefore consume the Seq:

- `+@foo`
- `.list`, `.List`, `.Array`, `.flat`, `.Slip`, `.eager`, `.join`
- iteration-producing APIs (`.iterator`, `for`, slurpy flattening that really
  wants element enumeration)

### 4.3 Cached Seq exception

Cached Seqs are multi-pass by design:

- cached Seq does not throw on re-iteration
- uncached Seq does

This distinction already exists in the implementation and should remain.

## 5. Choke points

The important thing is to stop fixing this at random callsites. The current
useful choke points are:

### 5.1 Consumed guard / consume transition

- `src/runtime/methods_dispatch_match2.rs`
  - `.iterator` is the cleanest "consume transition" point today
- `src/runtime/methods_call_dispatch.rs`
  - central guard for many forcing methods

These should become the authoritative places that move a Seq from "fresh" to
"consumed" for general iteration.

### 5.2 Single-argument-rule slurpy boundary

- `src/runtime/types/binding_signature.rs`
- `src/parser/stmt/sub_param/param_inner.rs`

This is where `+foo` vs `+@foo` must diverge.

The target behavior is:

- `+foo`: preserve one Seq object as one value
- `+@foo`: consume/flatten into list context

### 5.3 Helpers and test adapters that currently force

- `src/runtime/test_functions/comparison.rs`
- `src/runtime/builtins_operators_infix.rs`
- any helper that calls `value_to_list` / `.to_vec()` / `Array(...)` on Seq

These are the places most likely to accidentally bypass consumed state.

## 6. Design direction

### 6.1 Introduce an explicit read taxonomy

Instead of reasoning per-method ad hoc, we should classify Seq reads as:

- **preserve**: keep the Seq object
- **iterate**: obtain an iterator and mark consumed if uncached
- **force**: fully materialize to a List/Array

This can be implemented as shared helpers, for example:

- `seq_preserve(value) -> Value`
- `seq_iterator(value) -> Iterator | X::Seq::Consumed`
- `seq_force_list(value) -> Vec<Value> | X::Seq::Consumed`

The exact function names do not matter. The separation does.

### 6.2 Stop letting generic `value_to_list` decide Seq semantics

`value_to_list`-style helpers are convenient, but they are too lossy as the
main Seq interface because they erase:

- whether the source was a Seq or Array
- whether the Seq was cached
- whether iteration should consume

The design target is:

> generic listification should be used only at deliberate force points

### 6.3 Keep consumption state on the Seq identity, not the iterator only

The consumed bit must live on the Seq's shared identity, not only on a
transient iterator object. Otherwise:

- two consumers can each obtain a "fresh" iterator
- helpers that bypass `.iterator` can accidentally duplicate the stream

The existing `seq_is_consumed` / `seq_consume` substrate suggests this is
already the intended model. The campaign should reinforce it rather than add a
parallel one.

## 7. Implementation slices

### Slice 1 — codify preserve vs force at slurpy binding

Goal:

- `+foo` preserves Seq
- `+@foo` forces list context

Files:

- `src/runtime/types/binding_signature.rs`
- `src/parser/stmt/sub_param/param_inner.rs`

Why first:

- it is user-visible
- it gives a precise pin for the semantic split
- it does not require solving every forcing API first

#### What the parser already does

The parser side is not the main blocker here.

- `src/parser/stmt/sub_param/param_inner.rs`
  - `+@a`, `+%h`, `+$x`, `+&f` set `onearg = true`
  - sigilless `+foo` sets:
    - `p.slurpy = true`
    - `p.onearg = true`
    - `p.sigilless = true`

So the syntax layer already preserves enough information to distinguish:

- one-arg slurpy
- sigilled vs sigilless one-arg slurpy

The semantic loss happens later.

#### Where the runtime currently collapses the distinction

The relevant implementation is in:

- `src/runtime/types/binding_signature.rs`

Today, the `pd.onearg` branch does this:

1. gather remaining positional args into `items`
2. build `let slurpy_value = Value::real_array(items)`
3. for sigilless `+foo`, bind that array-like value under the bare name
4. for `+@foo`, bind the same array-like value under an `@` name

That means the runtime is currently preserving only:

- "onearg slurpy happened"

but not:

- "this binding should preserve a single Seq object"

This is exactly why `+foo` and `+@foo` collapse to the same materialized shape.

#### Concrete Slice 1 plan

Slice 1 should be intentionally narrow:

1. **Do not change the parser representation**
   - `onearg` + `sigilless` is already sufficient
2. **Split the `pd.onearg` binding path in `binding_signature.rs`**
   - `sigilless +foo`
   - sigilled `+@foo`
3. **For `+foo`, preserve a single Seq argument as one value**
   - if there is exactly one remaining positional arg and it is `Value::Seq`,
     bind that Seq object directly
   - do not eagerly turn it into `Value::real_array(items)`
4. **For `+@foo`, keep forcing list context**
   - current `Value::real_array(items)` behavior is conceptually correct here
5. **Only broaden later**
   - Slice 1 is about the single Seq case first
   - broader Iterable-preserving behavior can be evaluated after the pins are green

#### Minimal rule for the first PR

For the first implementation PR, use the smallest possible rule:

- exactly one remaining positional arg
- arg is `Value::Seq`
- param is `pd.onearg && pd.sigilless`

Then:

- bind the original `Value::Seq` to the bare name
- keep the existing path for every other case

This avoids turning Slice 1 into a full redesign of slurpy flattening.

#### Why not preserve every Iterable immediately

Because that would mix three separate questions:

- Seq single-pass identity
- Range / lazy iterable preservation
- generic Iterable coercion semantics

The current blocker is specifically the Seq half. Preserve that first.

#### Candidate code shape

The implementation likely wants a helper in `binding_signature.rs`, e.g.:

- `bind_onearg_sigilless_preserving_seq(...)`

or a shared helper that returns:

- `PreservedSeq(Value)`
- `ForcedList(Value)`

Again, the exact function name does not matter. The important point is to make
the branch explicit, so later audits can see where `+foo` stopped being list-forcing.

#### Expected pins for Slice 1

- `+foo` receives one Seq and keeps `.WHAT === Seq`
- `+@foo` with the same argument yields List/Array semantics
- repeated consumption of the preserved Seq still goes through the consumed guard
- existing non-Seq one-arg slurpy cases stay unchanged

### Slice 2 — centralize forcing helpers

Goal:

- replace direct Seq materialization at broad helper boundaries with explicit
  preserve/iterate/force helpers

Files:

- `src/runtime/methods_call_dispatch.rs`
- `src/runtime/methods_dispatch_match2.rs`
- `src/runtime/builtins_operators_infix.rs`

### Slice 3 — audit helper/test/bridge surfaces

Goal:

- eliminate bypasses that silently re-iterate consumed Seq

Files:

- `src/runtime/test_functions/comparison.rs`
- other helper modules that inspect iterable values through listification

## 8. Validation plan

We need pins that are smaller than roast.

### Core pins

- first iteration succeeds, second iteration throws `X::Seq::Consumed`
- cached Seq allows repeated iteration
- `+foo` preserves `.WHAT === Seq`
- `+@foo` yields List semantics

### Regression surfaces

- slurpy signatures
- equality/comparison helpers
- list coercion methods
- any helper that previously forced Seq implicitly

## 9. Exit criteria

This track is done when:

- `roast/S06-signature/slurpy-params.t`'s remaining Seq half is no longer
  blocked on "materializes every Seq"
- forcing APIs share one consume model
- helper paths no longer silently bypass `X::Seq::Consumed`

At that point the remaining lazy-sequence failures can be treated as a pure
`lazy-arrays.md` problem.
