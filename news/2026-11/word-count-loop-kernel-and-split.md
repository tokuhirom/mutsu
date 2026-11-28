# Counting words into a hash runs as a loop kernel

`for $line.split(' ') -> $w { %counts{$w}++ }` (benchmarks/word-count.raku)
cost about 5,100 instructions per word. Three changes take it to about 2,300:

- **A named loop parameter its body cannot observe is bound in its slot
  only.** The body of a `-> $w` loop that passes the topic's allow-list
  (#14495: no call into user code, no closure, no bind or capture) and never
  writes `$w` reads it only through its fresh slot, which now has a hidden
  name of its own (`ForLoopSpec::param_slot_only`). The drivers skip the
  per-item env binding and readonly mark. The allow-list now admits reading a
  `%` element and `++`/`--` of one: a hash element is never an element of the
  source array. The JIT also steps `PostIncrementIndex`/`PostDecrementIndex`
  (the prefix forms already were), so such a body no longer runs interpreted.
- **A list kernel over `Str`s counts into a hash.** When every item is a plain
  `Str` and the loop variable is only used as the key of a hash element's
  `++`/`--`, the loop runs as a loop kernel (`ListKind::Str`): the variable is
  the item's index (`K::StrItem`), and `kernel_hash_incdec_item` reads the
  item as the key and steps the element exactly as the lean `%h{$k}++` does.
  The result is only sunk (`K::Sunk`). A present element that is not a plain
  `Int`, or an overflow, declines and the iteration reruns generically.
- **`split` by a string on ASCII text without a CR skips the grapheme index.**
  Every char of such a text is a grapheme of its own, so a byte-level hit is
  grapheme-aligned and a byte offset is a grapheme position; a one-byte
  separator is searched with `memchr`.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before (main d39be56a) | after |
|---|---:|---:|
| `benchmarks/word-count.raku` | 541,137,080 Ir | 264,094,349 Ir |

Its warm section measured about 26 ms locally, against rakudo 2026.11's
~35 ms and Raku++ v5.9.0's ~29 ms on the same box.
