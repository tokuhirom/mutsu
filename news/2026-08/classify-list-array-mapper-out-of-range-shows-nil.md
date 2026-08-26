# An array/hash classify mapper reports a miss as `Any`, not `Nil`

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/Baggy.rakudoc:197`):

```raku
my @mapper = <zero one two three four five>;
say MixHash.new.classify-list: @mapper, 1, 2, 3, 4, 4, 6;
```

`raku` gives `MixHash((Any) four(2) one three two)` — index `6` is out of range
for `@mapper`, so its classifier key is the `Any` type object. mutsu gave
`MixHash(Nil four(2) one three two)`: same grouping and counts, wrong key.

## Root cause

`builtin_classify` (`src/runtime/builtins_collection_classify.rs`) has a
dedicated branch for a *non-callable* mapper — an `Array` indexed by the item,
or a `Hash` subscripted by it — separate from the block-mapper branch that calls
the closure. That branch substituted a literal `Value::NIL` on a miss:
`values.get(idx).cloned().unwrap_or(Value::NIL)`.

That is the wrong stand-in. The read it is emulating (`@mapper[6]`, `%mapper<z>`)
yields the `Any` **type object**, and `raku` keys the bucket by exactly that —
which is why the block-mapper form (`.classify: { @mapper[$_] }`), where the
subscript really is evaluated, was correct all along.

## Fix

The array-index miss, the hash-key miss and the negative-index case now all go
through a single `mapper_miss()` helper returning the `Any` type object.
Verified against `raku` v2026.06 for all three: an out-of-range array index, a
missing hash key, and the `MixHash.classify-list` case from the ticket.

Pinned by `t/buf-and-list-mutators.t`.
