# `nqp::existspos` and the multi-dimensional positional ops

Part of the `nqp::` coverage campaign (#11488). The 27 array ops tracked by
#11493 are now resolved, and every one of them used to die with
`Unsupported nqp:: op`. 26 are implemented; the 27th, `list_b`, is recorded
as not applicable (see the end of this entry).

- `existspos` answers whether a slot holds a value. A negative index counts
  from the end. Holes left by a past-the-end store, deleted slots and null
  slots answer 0. It uses `ArrayData::hole_at`, the same predicate
  `@a[$i]:exists` reads, so the two cannot disagree.
- The 24 multi-dimensional ops are `atpos2d`/`atpos3d`/`atposnd` and
  `bindpos2d`/`bindpos3d`/`bindposnd`, each with its `_i`/`_n`/`_s` twin.
  They work on shaped arrays (`my @a[2;2]`, `array[int].new(:shape(2,2))`).
  mutsu stores a shaped array as nested rows, so each op walks to the row
  with the ordinary element read and then runs the matching
  *one-dimensional* op. `atpos2d_i($a, $i, $j)` is literally
  `atpos_i(atpos($a, $i), $j)`, so there is no second element-access
  implementation. A bind writes through to the array.
- `atposref_s` completes the `atposref_i`/`_n`/`_u` family.

The coverage script now has a "Not applicable" list for ops that no Raku
program can actually run, each with its evidence. Its first entry is
`nqp::list_b`. Rakudo rejects every Raku call to it at compile time ("needs
a list of blocks, got QAST::Op"), because a Raku block literal never compiles
to the bare `QAST::Block` the op requires.
