# `my %h is default(V)` — a `Nil` pair value in a whole-hash assignment decays to `Any`, not `V`

Split out of `todo/tickets/array-attribute-default-not-applied-on-nil-assign.md` while fixing the
array/attribute half of the same family (see
`news/2026-08/container-attribute-is-default-survives-whole-container-assignment.md`). The
attribute-side hash case now works; this lexical one does not.

## Minimal repro

```raku
my %h is default(42);
%h = (a => 1, b => Nil);
say %h;
```
- `raku`: `{a => 1, b => 42}`
- `mutsu`: `{a => 1, b => (Any)}`

The array counterpart is correct in mutsu (`my @a is default(42); @a = (1, Nil, 3)` → `[1 42 3]`),
and so is the *attribute* hash form (`class F { has %.h is default(42) is rw }; $f.h = (a => 1, b =>
Nil)` → `{a => 1, b => 42}`).

## Root cause

`decay_nil_hash_value` in `src/runtime/utils/coerce_containers.rs:21` hardcodes `Any`:

```rust
fn decay_nil_hash_value(v: Value) -> Value {
    if v.is_nil() { Value::package(Symbol::intern("Any")) } else { v }
}
```

Its doc comment states the assumption explicitly — "these builders never see a pre-existing
typed/`is default(...)` container to decay against, so `Any` is exactly what
`Interpreter::typed_container_default` would compute here anyway". That assumption is false when the
*assignment target* carries `is default(...)`: `build_hash_from_items` runs during
`coerce_hash_var_value`, so the `Nil` is already `Any` by the time the assignment could consult the
target's default. The array path has no equivalent early decay (a parenthesised pair list is a
`List`, and `typed_container_default` returns `Nil` — "no decay" — for a `List`), which is why only
the hash sigil is affected.

The attribute path avoids the problem because `normalize_rw_accessor_assignment` uses
`normalize_hash_like_assignment`, a different builder that preserves `Nil` long enough for
`decay_nil_container_elements` to decay it against the (now-carried) container default.

## Why it is not a one-liner

`build_hash_from_items` / `build_hash_from_items_with_key_coercion` are pure value-level helpers with
many callers and no access to the assignment target, so the fix is to thread the target's default
(or defer the decay to the assignment site, the way the array path does) rather than to patch
`decay_nil_hash_value` in place. That is a wider change than the attribute fix it was found
alongside, hence this separate ticket.

## Affected files

- `src/runtime/utils/coerce_containers.rs` (`decay_nil_hash_value`, `build_hash_from_items`)
- `src/vm/vm_var_assign_coerce.rs` (`coerce_hash_var_value` — the assignment-side caller)

## Pin when fixed

`t/attribute-default-array-nil-reset.t` has a comment marking where the assertion belongs.
