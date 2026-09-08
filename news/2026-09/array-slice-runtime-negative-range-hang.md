# An array slice whose range end came from a negative variable hung forever

`my @el = (4,); my $e = -1; say @el[0 .. $e]` never returned. The Range itself
was always right — `(0 .. $e).elems` is `0` and `(0 .. $e).raku` is `0..-1` —
so the fault was entirely in the subscript, and only with a *runtime* endpoint:
the literal spelling `@el[0 .. -1]` is rejected up front by both mutsu and raku
("Unsupported use of a negative -1 subscript to index from the end"), which is
why the hole survived so long unnoticed.

## Root cause

The inclusive-range subscript arms computed their end index as
`b.max(-1) as usize`. For a negative `b` that is `(-1) as usize`, i.e.
`usize::MAX`, and the slice loop then ran `for i in start..=usize::MAX`,
resolving each out-of-bounds slot to the container's typed default and pushing
it. The process grew a list of `Any`s until it was killed. A hang is worse than
a wrong answer — it takes the whole process out with no diagnostic — and
`0 .. $n-1` over a computed length is one of the most common slice idioms
there is; this was the real cause of the `Language/objects.rakudoc:1397`
doc-diff timeout that had been blamed on the BinaryTree role example.

Three arms in `src/vm/vm_var_index_ops.rs` carried the same cast: the `Array`
one (which hung), and the `Seq` and `Range`-target ones, which were guarded
against the runaway loop by a `min(len - 1)` clamp but therefore answered the
*whole* list where raku answers the empty one.

## The fix

All three now go through one helper, `Interpreter::inclusive_range_window`,
which resolves `a .. b` to a half-open `[start, end)` window entirely in `i64`
before any `usize` cast, and answers `None` when the range is empty. Doing the
comparison in signed arithmetic is what makes the negative end simply empty
instead of enormous.

Two behaviours fell out of routing every arm through the one helper:

- `@a[0 .. *]` on an *empty* array is now `()`. The old unbounded-end
  expression was `items.len().saturating_sub(1)` used as an *inclusive* end, so
  a zero-length array produced the window `0..=0` and the slice answered
  `(Any,)`. raku answers `()`.
- A negative slice *start* now throws `X::OutOfRange` ("Index out of range.
  Is: -1, should be in 0..^Inf") instead of silently clamping to 0. That is
  what raku does — `my $s = -1; @a[$s .. 2]` is an error there, exactly as a
  plain `@a[$s]` read is — and mutsu used to answer a quietly wrong `(1, 2, 3)`.

## Pin

`t/array-slice-runtime-negative-range.t` covers the reported repro, the
`List`/`Seq`/itemized-array/`comb`/`Range`-target and associative twins, the
negative-start throws, and the ordinary slice shapes (padding, unbounded end,
`*-1` counting, the exclusive-end twin) as controls. Its last assertion is a
`Test::Util` `doesn't-hang` run of the original repro in a child process, so a
regression fails the test instead of wedging the suite. The whole file also
passes unmodified under real Rakudo.

Closes [#7578](https://github.com/tokuhirom/mutsu/issues/7578).
