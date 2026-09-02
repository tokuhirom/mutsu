# A hash now carries whether its values are containers, so `.VAR` can answer

ADR-0040 slice 4b split hash construction in two: `Value::hash` itemizes its
values (a Raku `Hash` stores each value in a `Scalar` container) and
`Value::hash_bare_values` does not, for the associative things mutsu represents
with the same `Value::Hash` repr but whose values raku says are **not**
containers — a `Map`, a `Match`'s capture map, a slurpy `*%h`/`%_`, a
`Capture`'s `.hash`.

`.raku` and list-context arity followed that split immediately. `.VAR` could not:

```
sub slurped(*%h) { %h<a>.VAR.^name }
say slurped(a => ('x', 'y'));   # raku: List     mutsu: Scalar
```

## Why it could not be answered before

`Value::elements_are_containers` — ADR-0040 slice 3's one-place discriminator,
and what `.VAR` reads — answered `ValueView::Hash(_) => true` unconditionally.
An array carries the same distinction in its `ArrayKind` tag (`List` vs
`Array`); a hash had no such tag, so there was nowhere to read the bit from.

It cannot be answered from the *value* either: a real `Hash` holding a plain
`Int` must still say `Scalar`, and a plain `Int` is never itemized.

## The fix

`HashData` grows the missing bit, `bare_values: bool`, the hash-side twin of
`ArrayKind`'s `List`/`Array` distinction. `Value::hash_bare_values` sets it;
`Value::elements_are_containers` and the interpreter's
`container_elements_are_containers` read it. `Value::hash` reads it too, so a
bare-valued hash keeps its values bare through every later rebuild —
copy-on-write, `Map` re-tagging, `set_hash_original_keys` — all of which route
back through that constructor.

Two producers that had been getting the right answer only by accident now set
the bit outright rather than leaning on the `declared_type == "Map"` check:
`Capture.hash`, and `.Map` on a non-`Hash` receiver.

Only one place constructs a `HashData` literally (`HashData::new`), so the field
cost nothing to thread through; `Default` and `Clone` carry it everywhere else,
and `PartialEq` still compares only the map, so hash equality is unchanged.

## Verification

`t/element-store-itemization.t`'s slice-4b section now pins `.VAR` alongside
`.raku` and arity for every bare-valued shape — a slurpy `*%h` (`List`), a
`Map.new` value (`List`), a `Match`'s quantified capture (`Array`), a
`Capture`'s `.hash` (`List`) — against a real `Hash`'s `Scalar`, all dual-oracled
against `raku` v2026.07. Full `t/` suite and the roast whitelist pass.

Closes `todo/tickets/var-on-a-bare-valued-hash-answers-scalar.md`, filed by
ADR-0040 slice 4b as its own named residue.
