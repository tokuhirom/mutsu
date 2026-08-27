# `.grep` over an array with `:delete` holes materializes them, breaking later trailing-slot truncation

Found while fixing the twin bug in `.map`
(`news/2026-08/native-array-map-loop-was-9x-slower-than-the-shared-loop.md`).
The `.map` half is fixed; `.grep` still has it.

## Root cause

`ArrayData` records deleted slots in an `initialized` bitmap, and
`@a[$i]:delete` on the *last* element truncates the array past any trailing
holes. Running a `.grep` over the array in between destroys that: the grep rw
path promotes each matched source slot to a shared `ContainerRef` cell and
writes the whole vector back as the new source
(`runtime/methods_collection_ops/grep.rs`, the `ValueView::Array` arm →
`overwrite_array_bindings_by_identity`). The rebuild goes through
`Value::array_data_like`, which *does* copy `initialized` across — but the
promoted cells overwrite slot contents regardless, and the array that comes back
no longer reads a deleted slot as a hole, so the subsequent `:delete` sees a live
trailing element and does not truncate.

The `.map` fix was to leave the source container completely alone when the block
wrote nothing back. The same gate is the likely shape here, but grep's writeback
is not purely conditional — it exists to establish the rw aliasing
(`@a.grep(...)>>++` must update `@a`), so it cannot simply be skipped. Worth
checking first whether promotion can preserve the hole bits, or be limited to
slots that are actually initialized.

## Repro

```raku
my @d = <a b c d>;
@d[2]:delete;
@d.grep({ True }).join(",");   # remove this line and the next assert passes
@d[3]:delete;
say +@d;   # mutsu: 3   raku: 2
```

`roast/S32-array/delete.t` does not currently catch this (it uses `.map`, which
is now fixed), so there is no failing spec test — but the divergence from raku is
real.

## Affected files

- `src/runtime/methods_collection_ops/grep.rs` — the `ValueView::Array` arm.
- `src/runtime/resolution_map_grep_rw.rs` — `eval_grep_over_items_with_mutated`
  (its `.map` sibling now reports whether it wrote anything back; grep could do
  the same).
