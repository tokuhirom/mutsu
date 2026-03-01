# TODO: @-variable Array/List consistency for eqv

## Background

The `eqv` operator was fixed (PR #682) to correctly distinguish Array (mutable=true)
from List (mutable=false) per the Raku spec (raku-doc: Language/operators.rakudoc "infix eqv"):

```
1 eqv 1.0      # False (Int vs Num)
[1,2] eqv (1,2) # False (Array vs List)
```

This exposed a deeper issue: `@`-sigiled variables should always be Array containers
(mutable=true), but many internal operations produce Lists (mutable=false).

## Problem

In Raku, `my @a = expr` always creates an Array container (see raku-doc: Language/containers.rakudoc).
In mutsu, `Value::Array(items, bool)` uses the second field to distinguish:
- `true` = Array (from `[...]` literals, `Value::real_array()`)
- `false` = List (from `Value::array()`, most internal operations)

`coerce_to_array()` preserves existing Array mutability but wraps new values with
`Value::array()` (List). This means:
- `my @a = [1,2,3]` → Array (true) -- correct
- `my @a = 1,2,3`   → List (false) -- WRONG, should be Array
- `my @a = func()`   → List (false) -- WRONG, should be Array

## Root cause

Raku uses Scalar containers (`$`) for itemization: `@arr` flattens in argument position,
`[1,2]` (itemized) doesn't. mutsu uses Array/List distinction as a proxy, conflating:
- Container type (Array vs List) — structural
- Itemization (should flatten or not) — contextual

## Approach: Slip-based flattening

Instead of adding `Value::Scalar` (which would require touching every `match Value::Array`),
use `Slip` to mark "flatten this" at the compiler level:

- `@var` reference in argument position → compiler emits `MakeSlip` after loading the value
- `[1,2]` literal → stays as `Array(items, true)`, not wrapped
- Slurpy params / `from-list` / etc. check for `Slip` instead of `Array(_, false)`

## Implementation plan

Each step is designed to be completable in a single AI session with all tests passing.

### Step 1+2: Slip-wrap @-variable references + update flattening logic (combined)

These must be done together since changing one without the other breaks tests.

**Compiler change** (compiler/expr.rs):
- When compiling `Expr::ArrayVar` in argument position, emit `MakeSlip` after
  the variable load to wrap the value in `Slip(items)`
- `[...]` literals are NOT wrapped — they stay as `Array(items, true)`

**Update 20 flattening sites** that currently check `Array(_, false)`:
- `src/builtins/functions.rs` (1) — `flat` function
- `src/builtins/methods_0arg/mod.rs` (1) — Uni codepoint detection
- `src/runtime/builtins_collection.rs` (2) — `reverse`, `sort` result
- `src/runtime/builtins_io.rs` (1)
- `src/runtime/methods.rs` (6) — `from-list`, `.WHAT`, augmented Array dispatch
- `src/runtime/methods_collection_ops.rs` (3)
- `src/runtime/test_functions.rs` (2) — `is-deeply`
- `src/runtime/utils.rs` (3) — `value_type_name`, `coerce_to_array`
- `src/value/mod.rs` (1)

For each site, determine if the `false` check means:
- (a) "flatten this" → change to check `Slip` instead
- (b) "this is a List type" → keep as-is (type name, `.WHAT`)
- (c) "produce a List result" → keep as-is (reverse/sort return Lists)

### Step 3: Fix coerce_to_array + mutating methods

- `coerce_to_array()` → always produce `Array(items, true)` (~5 lines)
- `push`/`pop`/`shift`/`unshift`/`splice`/`append`/`squish` in methods_mut.rs
  → use `Value::real_array()` instead of `Value::array()` (~10 lines)
- `exec_set_local_op` LazyList conversion → `Value::real_array()` (1 line)
- Restore 4 roast tests to whitelist

### Step 4: Fix .raku output + is-deeply

- `raku_value()` in methods_0arg/mod.rs: Array=`[...]`, List=`(...)`
- Update local tests expecting `$[...]` for @-variables
- Switch `is-deeply` from `==` to `eqv` in test_functions.rs
- Restore remaining roast tests to whitelist if any

## Codebase statistics

- `Value::array()` (creates List): **548 occurrences** across 47 files — NOT changed
- `Value::real_array()` (creates Array): 10 occurrences across 7 files
- `Value::Array(_, false)` pattern matches: **20 occurrences** across 9 files — reviewed in Step 1+2

## Roast tests removed from whitelist (PR #682)

- `roast/S02-literals/listquote.t` — `{...}<a b c>` eqv comparison
- `roast/S06-currying/assuming-and-mmd.t` — assuming with eqv checks
- `roast/S06-currying/misc.t` — currying eqv result check
- `roast/S06-currying/slurpy.t` — slurpy assuming with eqv (18 failures)

These will pass again after Step 3.
