# `Pair.Hash` on a Pair with a list value dies with "Odd number of elements"

```
raku  -e 'say (a => (1,2)).Hash.raku'   # {:a($(1, 2))}
mutsu -e 'say (a => (1,2)).Hash.raku'
# Odd number of elements found where hash initializer expected:
# Only saw: a	1 2
```

A `Pair` whose value is a `List` reaches the Hash coercion as a flattened
three-element list (`"a", 1, 2`) instead of as one key/value pair, so the
odd-element check fires. A Pair with a scalar value (`(a => 1).Hash`) works.

## Where

`crate::builtins::map_hash_coerce::to_hash` (`src/builtins/map_hash_coerce.rs`)
has a `ValueView::Pair` arm in its `_` fallback, but the receiver is arriving as
an `Array`/`Slip` view first (the `ValueView::Array(..) => items_to_hash(...)`
arm), which flattens the pair's list value into the item stream. `items_to_hash`
then sees three items and raises the odd-element error. The fix is either to
stop the flatten for a single-`Pair` receiver, or to have `items_to_hash` treat a
`Pair` item as one key/value binding rather than spilling it.

## Provenance

Found while verifying ADR-0040 slice 4b (2026-09-02) and confirmed **pre-existing**
by rebuilding `main` — it is not a slice-4b regression. Unrelated to the
itemization work, filed separately so it does not get lost.
