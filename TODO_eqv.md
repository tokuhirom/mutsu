# TODO: @-variable Array/List consistency for eqv

## Background

The `eqv` operator was fixed to correctly distinguish Array (mutable=true) from List (mutable=false)
per the Raku spec (raku-doc: Language/operators.rakudoc "infix eqv"):

```
1 eqv 1.0      # False (Int vs Num)
[1,2] eqv (1,2) # False (Array vs List)
```

However, this exposed a deeper issue: `@`-sigiled variables should always be Array containers
(mutable=true), but many internal operations produce Lists (mutable=false).

## Problem

In Raku, `my @a = expr` always creates an Array container (see raku-doc: Language/containers.rakudoc).
In mutsu, `Value::Array(items, bool)` uses the second field to distinguish:
- `true` = Array (from `[...]` literals, `Value::real_array()`)
- `false` = List (from `Value::array()`, most internal operations)

The problem is that `coerce_to_array()` (called during @-variable assignment in VM) preserves
existing Array mutability but wraps new values with `Value::array()` (List). This means:
- `my @a = [1,2,3]` → Array (true) -- correct
- `my @a = 1,2,3`   → List (false) -- WRONG, should be Array
- `my @a = func()`   → List (false) -- WRONG, should be Array

## Why not just fix coerce_to_array?

Changing `coerce_to_array()` to always produce `true` was attempted but causes cascading failures:

1. **Slurpy parameter flattening**: `sub f(*@v) { }; f(@a)` — slurpy params flatten Lists but
   not Arrays. With @-vars as Arrays, they stop flattening.
2. **Supply.from-list**: Uses `Array(_, false)` to detect "flatten this argument". Without Scalar
   containers, can't distinguish `@arr` (flatten) from `[1,2]` (don't flatten).
3. **.raku output**: Array renders as `[...]`, List as `$[...]` (old behavior was wrong but tests
   depended on it). Correct behavior: Array=`[...]`, List=`(...)`.
4. **Mutating methods** (push/pop/shift/splice): Produce Lists via `Value::array()`.

## Root cause

Raku distinguishes "flatten in argument list" vs "preserve as single item" using **Scalar containers**
(`$`). `@arr` is not in a Scalar → flattens. `[1,2]` is itemized → doesn't flatten.
Without Scalar containers, mutsu uses Array/List distinction as a proxy, which conflates two
independent concepts:
- Container type (Array vs List) — structural
- Itemization (Scalar wrapping) — contextual

## Implementation plan

### Phase 1: Scalar container basics
1. Add `Value::Scalar(Box<Value>)` variant or use a wrapper mechanism
2. `[...]` literals produce `Scalar(Array(...))` — itemized, won't flatten
3. `@`-variable values are bare `Array(...)` — not itemized, will flatten
4. Update argument passing to flatten non-Scalar Arrays, preserve Scalar-wrapped values

### Phase 2: Fix coerce_to_array
1. Change `coerce_to_array()` to always produce `Array(items, true)`
2. All internal operations that produce array-like values for @-variables use `real_array()`
3. Mutating methods (push/pop/shift/splice/append) use `real_array()`

### Phase 3: Fix .raku output
1. Array `.raku` → `[...]`
2. List `.raku` → `(...)`
3. Scalar `.raku` → `$[...]` or `$(...)` (itemized prefix)
4. `.item` returns `Scalar(value)`

### Phase 4: Fix is-deeply
1. Switch `is-deeply` from `==` (PartialEq) to `eqv` (structural equivalence)
2. Handle `Seq:D` → List conversion per raku-doc (Type/Test.rakudoc)

## Roast tests removed from whitelist

These tests were passing before but now fail due to the eqv fix exposing Array/List inconsistency:

- `roast/S02-literals/listquote.t` — `{...}<a b c>` eqv comparison
- `roast/S06-currying/assuming-and-mmd.t` — assuming with eqv checks
- `roast/S06-currying/misc.t` — currying eqv result check
- `roast/S06-currying/slurpy.t` — slurpy assuming with eqv (18 failures)

These will pass again once @-variables consistently produce Array (Phase 2+).
