# `.grep` over an array no longer materializes `:delete` holes

The `.grep` twin of a bug fixed for `.map` a day earlier
(`news/2026-08/native-array-map-loop-was-9x-slower-than-the-shared-loop.md`).
Running a `.grep` over an array with a deleted slot silently turned that hole
into a live element, so a later trailing-slot `:delete` stopped truncating:

```raku
my @d = <a b c d>;
@d[2]:delete;
@d.grep({ True });   # remove this line and the next assert passes
@d[3]:delete;
say +@d;             # was: 3    raku: 2
```

## Root cause

`.grep` over an array promotes each *matched* source slot to a shared
`ContainerRef` cell and hands the same cells to the result, so a writeback loop
(`@a.grep(...)>>++`, `for @a.grep(...) { $_++ }`) mutates through into the
source. That promotion applied to every matched slot — including a hole.

`ArrayData::hole_at` recognises a hole by two things together: the gap marker
value (`Package("Any")`, or the container's declared element type) sitting in
the slot, **and** the slot's absence from the `initialized` set. Wrapping that
marker in a `ContainerRef` changes the value's view, so `hole_at` saw a live
element while `initialized` still called the slot empty. The two disagreed, and
`trim_trailing_array_holes` — which walks back from the end while `hole_at`
holds — stopped at the promoted slot.

Note this was *not* the metadata-dropping shape the `.map` bug had: grep already
rebuilds the source through `Value::array_data_like`, which copies `initialized`
faithfully. The bit that was lost was the marker value in the slot.

## Fix

Skip holes in the promotion loop. A `:delete`d slot has no element container to
alias, and promoting it *creates* one where the array says none exists — so the
grep result gets the raw marker instead, which is what Raku yields there (`Any`),
rather than an alias into a slot that does not exist. Everything else about the
promotion is unchanged, so the writeback aliasing the mechanism exists for still
works, including when an unmatched hole sits in the middle of the source.

## Verification

`t/grep-array-delete-holes.t` (12 tests, validated against `raku` first) pins
both halves: the trailing-`:delete` truncation across matcher shapes that do and
do not accept the hole (block, regex, always-false, unconsumed result), the hole
still appearing in the grep result as an undefined value, and the three
writeback behaviours (`>>++`, a `for` loop, and a named copy *not* writing back)
including with a hole present. The `t/` suite (3502 files) and a 173-file
whitelisted roast sweep of every file mentioning `grep`/`:delete`/`:exists`
pass.
