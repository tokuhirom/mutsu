# `classify-list` with an array mapper renders an out-of-range key as `Nil` instead of `(Any)`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Baggy.rakudoc:197`).

## Repro

```raku
my @mapper = <zero one two three four five>;
say MixHash.new.classify-list: @mapper, 1, 2, 3, 4, 4, 6;
```

- raku: `MixHash((Any) four(2) one three two)` — index `6` is out of range for `@mapper`
  (valid indices `0..5`), so its classifier key is the `Any` type object, gisted as `(Any)`.
- mutsu: `MixHash(Nil four(2) one three two)` — same element ordering and counts, but the
  out-of-range key renders as `Nil` instead of `(Any)`.

## Isolating the root cause

A direct out-of-bound array read is correct on its own:

```raku
my @mapper = <zero one two three four five>;
say @mapper[6];            # mutsu: (Any)  -- correct
say @mapper[6].^name;      # mutsu: Any    -- correct
```

And `.classify` with an explicit block mapper is also correct:

```raku
say (1, 2, 3, 4, 4, 6).classify: { @mapper[$_] };
# mutsu: {(Any) => [6], four => [4 4], one => [1], three => [3], two => [2]}   -- correct
```

So the bug is specific to `classify-list`'s **array-as-mapper** form (`classify-list: @mapper,
LIST`, as opposed to `classify: BLOCK, LIST` or `classify-list: BLOCK, LIST`). This suggests
`classify-list`'s array-mapper implementation substitutes a literal `Nil` for an out-of-range
index result instead of using the array element read it actually got (which would already be
the correct `Any` type object, per the two working cases above) — likely a `@mapper[$_] //
Nil`-shaped defensive fallback where none should exist, or a distinct code path that doesn't
reuse the plain indexed-read logic at all.

## Affected files (starting point)

- `classify-list` implementation (likely `runtime/methods.rs` or a dedicated
  classify/categorize helper) — find the array-mapper branch specifically (as opposed to the
  block-mapper branch, which is correct) and look for a `Nil`-substituting fallback on missing
  array indices.
