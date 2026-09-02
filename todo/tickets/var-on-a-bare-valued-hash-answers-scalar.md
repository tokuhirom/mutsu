# `.VAR` on a bare-valued hash answers `Scalar` instead of the value's own type

ADR-0040 slice 4b split hash construction in two: `Value::hash` itemizes its
values (a Raku `Hash` stores each value in a `Scalar` container) and
`Value::hash_bare_values` does not, for the associative things mutsu represents
with the same `Value::Hash` repr but whose values raku says are **not**
containers — a `Map`, a `Match`'s capture map, a slurpy `*%h`/`%_`.

`.raku` and list-context arity now follow that split correctly. `.VAR` does not:

```
sub slurped(*%h) { %h<a>.VAR.^name }
say slurped(a => ('x', 'y'));   # raku: List     mutsu: Scalar
```

## Root cause

`Value::elements_are_containers` (`src/value/value_methods_a.rs`, ADR-0040
slice 3's one-place discriminator) answers `ValueView::Hash(_) => true`
unconditionally. For arrays the answer comes from the `ArrayKind` tag, which
distinguishes a real `Array` from a `List`; a `Hash` has no such tag, so there is
nowhere to read "my values are containers" from.

It cannot be answered from the *value* either: a real `Hash` holding a plain
`Int` must still say `Scalar`, and a plain `Int` is never itemized.

## What to do

Give `HashData` the missing bit — e.g. `pub bare_values: bool`, set by
`Value::hash_bare_values` — and have `elements_are_containers` read it. Only
four places construct a `HashData` literally (`value_collections.rs`,
`value_gc.rs` ×2, `nanbox/tests.rs`), so the field itself is cheap; the care is
in the paths that rebuild a hash's data (copy-on-write, `.Map` tagging,
`set_hash_original_keys`) which must carry the bit through rather than reset it.

`Capture.hash` (`src/builtins/methods_0arg/mod.rs`) builds its `HashData`
directly via `Value::hash_with_data` and is bare-valued too, so it needs the bit
set as well.

## Why it is not urgent

This is pre-existing: before slice 4b the read-side compensator turned the bare
`List` into an `ItemList`, so `.VAR` answered `Scalar` then too — but so did
`.raku` and arity. Slice 4b fixed two of the three; this is the third.

Related: `docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md` (slice
4b), `todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`.
