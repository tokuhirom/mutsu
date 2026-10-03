# Holes in an `is default` array read the default through every view

`my @m is default(7) = 1, 2, 3; @m[0]:delete; say @m.values` printed
`(Any 2 3)`, and so did `.kv`, `.map`, `for @m`, `@m[*]`, `"@m[]"`,
`my @c = @m` and some twenty other views. Only `@a[$i]` read the default
(#10360). The earlier fix (#10317) patched a handful of views one by one, and
70+ other readers still saw the raw `Any` hole marker.

The fix changes how a hole is stored. A hole in an array with a non-`Nil`
`is default(...)` now holds the default value itself (`ArrayData::gap_fill`),
so every reader that looks at the slots directly gets the right value.
Whether a slot is a hole is decided only by the array's `initialized` record.
That record had gaps, and those are fixed too:

- `push`, `append`, `pop`, `insert`, `remove`, `truncate` and `drain` keep it
  in step. A pushed `Any`, or a pushed `7` on a `default(7)` array, now
  exists.
- A list assignment copy (`my @c = @m`, `@a = @b`, an `@a is copy`
  parameter) settles the holes into present elements. It keeps the target's
  own default instead of taking the source's: `my @e is default(6) = @m`
  used to read `7` from a new hole.
- `:exists` trusts the array's own record. The name-keyed deleted-index table
  did not move with `shift`, `unshift` or `splice`, and an element write did
  not clear it.
- `for @a` and `.values` alias slots into element cells. A slot aliased this
  way is still a hole until something is written through the cell.
- `@a[5]++` past the end records its write, so the gap it grows is a hole.

Two differences remain, and both are Rakudo artifacts. Rakudo's `Array.sum`
skips a hole and `.Array` dies on one, while mutsu counts the default. An
`is default(Nil)` array still renders a deleted slot as `Any` where Rakudo
shows `Nil`.
