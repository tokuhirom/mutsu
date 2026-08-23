# A container attribute's `is default(...)` now survives a whole-container assignment

`has @.bar is default(42) is rw` did not behave like `my @a is default(42)`. Assigning `Nil` through
the accessor left `[Any]` instead of resetting the array to `[42]`, and — as the investigation
turned up — the divergence was considerably wider than the reported symptom.

## What was actually wrong

The ticket guessed that the `@`-sigil path consulted a *generic* per-element default instead of the
attribute's declared one. The real cause was that the attribute assignment path did not assign
*into* the container at all: it built a brand-new one out of the right-hand side and threw the old
container's embedded `is default(...)` away. Everything downstream followed from that.

`Interpreter::normalize_rw_accessor_assignment` (`src/runtime/methods_mut.rs`) had an array arm that
was a bare `super::coerce_to_array(value)`, and a `!preserve_hash_entries` hash arm that likewise
built a fresh hash. Only the *element-writeback* hash arm carried `HashData::default` forward. So:

- `$f.bar = <x y>; $f.bar[10]` returned `(Any)` — raku returns `42`. The same held for `%`.
- `$f.bar = (1, Nil, 3)` stored a literal `Nil` element, where raku stores the default (and, with no
  `is default`, stores `Any`). A real `Array`/`Hash` element is a `Scalar` and cannot hold `Nil`, so
  storing one was a container-invariant violation, not merely a cosmetic difference.
- `$f.bar = Nil` produced `[Any]`, because `coerce_to_array` is deliberately type-blind and hardcodes
  `Any` for a bare `Nil` right-hand side. That also meant the existing scalar-attribute
  `assigned_value.is_nil()` guard just below could never fire for a container attribute — by then the
  value was an `Array`, not `Nil`.

A separate gap affected the attribute twigil. `@!a = Nil` inside a method compiles to a by-name
`SetGlobal`, which never ran the `is default(...)` hole fixup that the by-slot `SetLocal` path has,
so it produced `[Any]` too.

## The fix

Raku's `=` assigns *into* an existing `Array`/`Hash` rather than replacing it, so the container's own
traits survive. That rule is now applied at each store:

- A new `Interpreter::carry_container_default` re-attaches the outgoing container's embedded default
  to the freshly built one, and both container arms of `normalize_rw_accessor_assignment` use it
  (the element-writeback hash arm's hand-rolled copy of the same logic was folded into it).
- The array arm additionally intercepts a `Nil` right-hand side before `coerce_to_array` can flatten
  it to `Any`, producing the one-element `[default]` that raku's list-assignment semantics call for.
- The four rw-accessor store sites in `src/runtime/methods_mut_method_lvalue.rs` now run
  `decay_nil_container_elements` on the normalized value, so any `Nil` element the assignment stored
  decays to the container's own default (ADR-0049) — the attribute's `is default(...)` when it has
  one, `Any` otherwise.
- `Interpreter::array_assign_nil_container_default` handles the `@a = Nil` reset for both the
  `SetLocal` and `SetGlobal` stores, reading the default from the outgoing container and falling back
  to the name-keyed `var_default` (the only source for a private-only attribute, whose container is
  not tagged at construction). It re-tags the fresh container, so `@!a[5]` still yields the default
  afterwards.

Notably the new attribute path gets a case the older lexical path still gets wrong: an explicitly
assigned `Any` element stays `Any` (`$f.a = (1, Any, 3)` → `[1 (Any) 3]`, matching raku), because the
decay keys on `Nil` only rather than treating `Any` as a hole.

## Pin

`t/attribute-default-array-nil-reset.t` — 21 assertions covering the array attribute (out-of-range
read, `.VAR.default`, `= Nil`, default survival across a whole-array assignment, `Nil` inside an
assigned list, element `= Nil`), the private `@!attr` twigil, the hash attribute, the no-default
`Any` normalization for both sigils, and the scalar attribute as a regression guard. The whole file
passes verbatim under real `raku`.

## Left open

The lexical hash counterpart (`my %h is default(42); %h = (a => 1, b => Nil)`) still yields `Any`:
`decay_nil_hash_value` hardcodes it at hash-build time, before any assignment target is known. That
needs threading the target default through `build_hash_from_items`, which has many value-level
callers, so it was split out to
`todo/tickets/lexical-hash-default-not-applied-to-nil-pair-value.md`.
