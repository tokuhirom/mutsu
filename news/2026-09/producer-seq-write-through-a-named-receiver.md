# An element producer's `Seq` writes through when it is reached by name, too

`.values` / `.kv` / `.pairs` hand out the source element's own `Scalar`
container (ADR-0036 slice 3), so subscripting the resulting `Seq` and assigning
writes **through** the cell to the source array. The computed-target spelling
did that; the same `Seq` reached through a variable silently dropped the write:

```
$ raku  -e 'my @a = <A B>; (@a.values)[0] = "x"; say @a'          # [x B]
$ mutsu -e '...same...'                                           # [x B]  OK

$ raku  -e 'my @a = <A B>; my \s = @a.values; s[0] = "x"; say @a' # [x B]
$ mutsu -e '...same...'                                           # [A B]  X  silent
```

`my $s = @a.values` and `my \s := @a.values` were wrong the same way.

## Root cause

The cell write-through existed in exactly one place:
`exec_index_assign_generic_op`, the computed-target op, had an inline "if the
target is a `Seq` with element containers, assign through the cell" block. The
**named**-receiver op (`IndexAssignExprNamed`) had no such block, and everything
it dispatches to — the shared-var fast paths, the plain-hash fast path, the full
slow path — assumes an `Array`/`Hash` target. A `Seq` fell through all of them
and the write went nowhere, with nothing reporting it.

## The fix

The block moved into a shared helper,
`Interpreter::try_seq_element_cell_assign`, and both ops call it: the generic op
where its inline block used to be, and the named op before its fast paths (which
all assume an Array/Hash target), resolving the receiver from the baked target
slot or, failing that, from `env`. One mechanism now serves both spellings
instead of one spelling having it and the other silently not.

## Pins

`t/producer-seq-named-receiver-write.t`, 13 rows cross-checked against raku:
both computed-target spellings, four named-receiver spellings (`\s =`, `$s =`,
`\s :=`, and `.kv`), a later index, two writes through one `Seq`, the two reads
that must keep reading through the cell, and two negatives.

## What this ticket used to say, and what is still open

The file this closes was written by ADR-0036 slice 5's 69-row sweep on
2026-09-01 and listed five failing rows with a root-cause analysis pointing at
`exec_index_op_with_positional` normalizing a `Seq` through
`resolve_array_entry`. **Four of those five rows had since been fixed** by the
ADR-0036 slice 4/5 and ADR-0064 work (#7218-#7230), and that root cause no
longer described a failing case — the file was simply never re-checked. Only the
named-receiver row survived, and its cause was somewhere else entirely. Worth
remembering the next time a ticket's "where to start" section looks
authoritative: re-run every row before trusting the analysis.

Two things in the same area remain, neither a lost write:

- **`.VAR.^name` through a `$`-sigil receiver.** `my $s := @a.values; say
  $s[0].VAR.^name` is `Str` where raku says `Scalar` (the sigilless `\s`
  spelling is already right, and the write-through works for both). That is the
  reflection surface `todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`
  owns, not a store bug.
- **A non-producer `Seq` accepts a subscript assignment.** `my $s = @a.map(*+1);
  $s[0] = 99` rewrites the `Seq` in place where raku dies with "Cannot assign to
  an immutable value". Pre-existing and identical on `main`; recorded as the
  `todo`-marked row in the new test file, with a live assertion beside it that
  the write at least never reaches the source array.
