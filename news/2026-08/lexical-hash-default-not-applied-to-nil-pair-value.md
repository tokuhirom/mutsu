# `my %h is default(V)` now applies its default to a `Nil` pair value

```raku
my %h is default(42);
%h = (a => 1, b => Nil);
say %h;   # raku: {a => 1, b => 42}   mutsu was: {a => 1, b => (Any)}
```

Split out of `todo/tickets/array-attribute-default-not-applied-on-nil-assign.md`
while fixing the array/attribute half of the same family (see
`news/2026-08/container-attribute-is-default-survives-whole-container-assignment.md`).

## The rule, established against `raku` v2026.06

A `Nil` assigned into a hash element takes that element container's
`is default(...)` value — but only a `Nil` that survives *until the assignment*:

| RHS | raku |
|---|---|
| `%h = (a => 1, b => Nil)` | `{:a(1), :b(42)}` — the default applies |
| `%h = a => 1, b => Nil` | `{:a(1), :b(42)}` — same, bare comma list |
| `%h<x> = Nil` | `{:x(42)}` — direct element store, already worked |
| `%h = (a => 1, b => Any)` | `{:a(1), :b(Any)}` — an explicit `Any` is a value, not a hole |
| `%h = %(a => 1, b => Nil)` | `{:a(1), :b(Any)}` — the `%(...)` Hash already decayed the `Nil` |
| `%h = %src` (from another hash) | same — `%src` holds `Any`, not `Nil` |
| no default at all | `{:a(1), :b(Any)}` |

Only the first two diverged in mutsu.

## Root cause

`build_hash_from_items` (via `decay_nil_hash_value`,
`src/runtime/utils/coerce_containers.rs`) decays a pair value of `Nil` to `Any`
— correct in isolation, because a Hash value is a Scalar container and an untyped
hash's own default *is* `Any`. But that builder is a pure value-level helper with
many callers and no access to the assignment target, so when
`coerce_hash_var_value` ran it on the way into a `%h` that carries
`is default(42)`, the `Nil` was already gone and the target's default was
unrecoverable. The array sigil was unaffected because a parenthesised list is a
`List`, for which `typed_container_default` returns "no decay".

## Fix

The target's default belongs to the *assignment*, not to the value builder, so
`coerce_hash_var_value` (`src/vm/vm_var_assign_coerce.rs`) now substitutes it
into the incoming pair list before the builders run. A new
`substitute_nil_pair_values` helper replaces a `Nil` pair value (both `Pair` and
`ValuePair` flavours — ADR-0021 makes named-ness a call-site marker, not a
semantic difference here) with the default, leaving keys, non-pair items and
nested containers untouched. It runs only for an `Array`/`Seq`/`Slip` RHS and
only when the target actually carries a default, so the `%(...)`-RHS and
no-default rows above are unaffected — which is exactly what raku does.

This mirrors what the `@`-sigil path already does in `exec_set_local_op_inner`,
without widening `build_hash_from_items`'s signature across its many callers.

Pinned by `t/lexical-decl-and-autoviv.t`, which asserts every row of the table
above (including `:exists` and `.elems`, so a defaulted store cannot quietly add
a key).

## Residual, not fixed here

The `@`-sigil hole replacement in `exec_set_local_op_inner` treats a
`Package("Any")` element as a hole as well as `Nil`, so
`my @a is default(42); @a = (1, Any, 3)` yields `[1, 42, 3]` where raku yields
`[1, Any, 3]`. That is the array twin of the "an explicit `Any` is a value"
row above and is a separate, pre-existing divergence.
