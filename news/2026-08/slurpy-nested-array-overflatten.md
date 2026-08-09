# `*@` slurpy no longer over-flattens nested array literals

`sub f(*@c) {...}` called with a nested array literal fully flattened
every level instead of stopping at real Array elements:

```
sub f(*@c) { say @c.elems }
f [[1,2],[3,4]];
```

- mutsu (before): `4` (fully flattened: `1, 2, 3, 4`)
- raku: `2` (one level: the two inner Arrays, `[1,2]` and `[3,4]`)

This silently reduced test coverage in Cro's `t/http2-request-serializer.rakutest`:
a `sub test($request, $count, $desc, *@checks, ...)` called with
`[[check1..check4], [check1..check4]]` collapsed 2 groups of 4 checks
into 8 individual checks, so only "check 1" ever ran per frame — every
frame silently skipped checks 2-4.

## Root cause

In Raku, every *element of a real Array* lives in an implicit Scalar
container, so slurpy flattening (`*@`) expands Iterables recursively but
stops at anything inside a Scalar container — expanding a real Array
contributes its elements as-is (one level), while an uncontainerized
List keeps flattening fully. mutsu's `flatten_into_slurpy`
(`src/runtime/types/signature.rs`, the function `bind_function_args_values`
actually uses for `*@` parameter binding) recursed into any non-itemized
Array unconditionally, with no distinction between Array-kind (should
stop) and List-kind (should keep flattening) — because inner `[...]`
literal elements are stored as `ArrayKind::Array` (bare, not itemized),
mutsu has no representation of "this element lives in a Scalar
container" to stop on.

A sibling function used by a different flattening opcode
(`flatten_value_for_slurpy` in `src/vm/vm_value_helpers.rs`, feeding
`OpCode::FlattenSlurpy`) had the identical bug and needed the identical
fix.

## Fix

Narrow, semantics-first fix (the full itemization-at-construction
rework is a separate, much larger change). `flatten_into_slurpy` takes a
list of RAW (un-pre-processed) caller arguments — every call site (`*@`
parameter binding, `sprintf`, `catdir`/`catfile`, ...) passes either the
whole argument list or a single argument wrapped in a one-element slice.
For each value: an itemized container (`$(...)`, `$[...]`) is preserved
whole; a non-itemized **real Array** (`Array`/`Shaped`/`Lazy`, i.e. the
`[...]` constructor or an `@`-var) contributes its own elements to the
output *one level, and no further* — a nested Array element is itself
opaque, never decomposed; a **List** (uncontainerized) still flattens
fully, recursively.

One `*@`-binding call site in `bind_function_args_values`
(`src/runtime/types/binding_signature.rs`) diverged from every other
caller: it pre-extracted a real Array's elements itself before calling
`flatten_into_slurpy`, so the fixed function's own one-level extraction
double-unwrapped a nested-array argument back to the original bug. Fixed
by passing the raw argument through, matching every other call site —
verified against a real regression `sprintf("%d %d %d", @a)` introduced
mid-fix (`t/sprintf-slurpy-flatten.t`), which pins exactly this
divergence.

## Verification

- All eight probe-table cases (bare array-literal argument, `@`-array of
  arrays, explicit itemization, plain scalars, a scalar-bound array
  argument, a List of Lists, a mixed scalar+nested-array literal) now
  match raku's `@c.elems` count exactly. The one remaining difference
  (mutsu's `.raku` gist shows `[1, 2]` where raku shows the itemized
  `$[1, 2]`) is a pre-existing, separately-scoped cosmetic gap — mutsu
  does not yet model the Scalar container an Array element lives in.
- `t/http2-request-serializer.rakutest` (vendored Cro::HTTP2 suite): every
  frame now runs its full "check 1".."check 4" group instead of only
  "check 1".
- New pin: `t/slurpy-nested-array.t` (passes under both `mutsu` and
  `raku`).
- Whitelisted `S06-signature`/slurpy (43 files, 1071 subtests), list/array
  (7 files, 245 subtests), and all 18 whitelisted `sprintf`/`zprintf`
  roast files pass with no regressions. Full `make test` passes.
