# `duckmap` does not itemize the sublist it descended into

```raku
say (1, (2, 3)).duckmap(-> Int $x { $x * 10 }).raku;
say (1, [2, 3]).duckmap(-> Int $x { $x * 10 }).raku;
say (1, (2, 3).Seq).duckmap(-> Int $x { $x * 10 }).raku;
say (1, %(a => 2)).duckmap(-> Int $x { $x * 10 }).raku;
```

```
raku : (10, $(20, 30))   (10, $[20, 30])   (10, $(20, 30))   (10, ${:a(20)})
mutsu: (10, (20, 30))    (10, [20, 30])    (10, (20, 30).Seq) (10, {:a(20)})
```

When the block rejects an element, `duckmap` descends into it — and rakudo
itemizes what comes back, so the sublist is one element of the result rather
than something that can flatten. `duckmap_element`'s descend arms in
`src/runtime/builtins_collection_deepmap.rs` return the plain container
(`Value::array` / `Value::real_array` / `Value::seq` / a plain hash) instead.

`deepmap` gets this right for the Array and List cases (its `itemize_result`
parameter is exactly this distinction), so the fix is to give `duckmap_element`
the same treatment. Note rakudo itemizes the Seq descend into an itemized
*List*, not a Seq, and the top-level result is never itemized — only the
elements the walk descended into.

Found while fixing `todo/tickets/deepmap-on-a-range-does-not-map.md`
(`news/2026-08/range-is-iterable-for-the-map-family.md`): the new Range arm
inherits whatever the List arm does, so `t/deepmap-on-a-range.t` compares a
Range descend against the equivalent List descend rather than against a raku
literal. Tighten that assertion to raku's own output when this is fixed.

Pre-existing and not blocking a roast file today — it surfaced from a hand
probe, not from the real-`Test` sweep.
