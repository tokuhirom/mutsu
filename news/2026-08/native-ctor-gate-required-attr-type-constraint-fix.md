# Native default-constructor gate no longer reads `is_required` as a type constraint

`Interpreter::is_native_default_constructible` (`src/runtime/methods_object.rs`)
decides, per attribute, whether a class's default `.new` can take the fully-native
fast path instead of falling through to the interpreter-driven constructor. Its
per-attribute `$`-sigil branch bound `let type_constraint = &attr.is_required;`
— a carried-over mismatch from the original 7-tuple shape of `ClassAttributeDef`
(ADR-0019 D2's tuple→struct conversion preserved the bug exactly, per that PR's
"zero behavior change" scope), reading the *required-ness* flag
(`Option<Option<String>>`) as if it were the attribute's *type constraint*
(a plain `Option<String>`, which actually lives in the separate
`attribute_types` side table).

## What it actually did

Tracing both consumers of the mis-bound variable:

- An untyped, non-required `$` attribute: `is_required` is `None`, so the
  branch always returned `true` — no type gating happened here at all.
- An `is required` `$` attribute (the common case, no reason string):
  `is_required` is `Some(None)`, so `inner.as_deref()` is `None` and the
  branch returned `false` — wrongly disqualifying the *whole class* from the
  native path, even though the function's own docstring says `is required`
  should be "allowed through" (the native builder raises
  `X::Attribute::Required` itself for an unprovided required attribute).

Crucially, this was a **performance-only** bug, not a correctness one: the
function has a second, *correct* blanket check a few lines below —
`class_def.attribute_types.values().all(...)`, applying the exact same
`is_simple_native_ctor_constraint` / `native_scalar_default` /
`is_native_coercion_ctor_constraint` three-way test to every declared type
constraint regardless of sigil. That check already fully gated `$` type-
constraint eligibility; the buggy per-attribute branch was pure redundant
(and wrong) noise on top of it. Net effect: any class with an `is required`
scalar attribute silently missed the native fast constructor path and always
paid the slower interpreter-driven `.new`, with no observable behavior
difference (`t/native-ctor-required-attrs.t` already asserts the intended
native-eligible behavior and passed throughout, on the interpreter fallback).

## Fix

The `$` branch now simply returns `true` — the redundant, buggy check is
removed, and the already-correct blanket `attribute_types` check does all the
real gating. Verified via `raku` and mutsu producing identical output/errors
for typed and untyped `is required` attributes (`Foo.new()` raising
`X::Attribute::Required`, `Foo.new(x => 5)` succeeding), the existing
`t/native-ctor-required-attrs.t` suite (14 assertions), and the full local
`t/` suite (29,577 tests).
