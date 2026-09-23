# Str positional methods use a cached grapheme index

`#9140`: every positional `Str` method — `.chars`, `.substr`, `.index` /
`.rindex` / `.contains` with a position, `.indices`, `.substr-eq`, `.ord`,
`.starts-with` / `.ends-with` — cost O(n) in the length of the invocant, so the
ordinary "walk a string by index" loop was O(n²):

```raku
my $n = 20000; my $s = "あ" x $n;
for ^$n -> $i { $s.substr($i, 1) }   # ~10.6s at 20000, 4x per doubling
```

Two causes compounded. The invocant was copied (`to_string_value()`) where a
borrow would do, and a grapheme position was turned into a byte offset by
segmenting the whole string on every call — `grapheme_units` even built a `Vec`
with one entry per byte for plain ASCII.

## The fix

`src/builtins/grapheme_index.rs` adds `GraphemeIndex`, built in one pass over
the string:

- a **flat** string (ASCII without `\r\n`) stores nothing — byte offsets are
  grapheme offsets;
- anything else stores the byte offset of every 32nd grapheme, so a
  grapheme ↔ byte conversion segments at most 31 graphemes from the nearest
  checkpoint. Starting segmentation at a checkpoint is exact because UAX #29
  never looks back across a grapheme boundary.

The index is cached per `Str` allocation in a small per-thread LRU keyed by the
payload's address and holding a `Weak` to it. The `Weak` is what makes the key
sound: the allocation cannot be freed (so its address cannot be reused) while
the entry exists, and the in-place `~=` path uses `Arc::get_mut`, which refuses
to mutate a buffer that has a `Weak` and copies instead. Strings under 256 bytes
skip the cache — building their index is cheaper than probing it, and it keeps
short accumulators on the in-place append path.

Every method listed above now borrows the payload and resolves positions
through the index; `.indices` resumes each search at a byte offset instead of
re-concatenating the suffix per hit, and `.rindex` searches bytes backwards and
accepts only a hit that starts and ends on grapheme boundaries (the old code
compared windows of a fully segmented unit array).

Correctness fixes that rode along: `.substr-eq` and `.contains` with a
position counted **codepoints** while `.chars`/`.substr` count graphemes, so
`"q\x[301]a".substr-eq("a", 1)` was `False` (raku: `True`); and `.index` /
`.indices` accepted a byte-level hit inside a grapheme, so
`"q\x[301]".index("q")` was `0` (raku: `Nil`) — they now skip such hits the
way `.rindex` already did.

`scripts/str-complexity-check.sh` measures each case at N and 2N calls and
prints the ratio (≈2 linear, ≈4 quadratic); the "after" column was taken with
ten times the base N (`SCALE=10`). Release build, 4-core container.
"Before" is the measurement recorded in the issue (not re-run here), at a
tenth of the N used "after":

| case (N calls on an N-char string) | before: N, t(2N), ratio | after: N, t(2N), ratio |
|---|---|---|
| `"あ" x N` → `.chars` | 10000, 11.04s, 3.81 | 100000, 0.07s, 1.24 |
| `"あ" x N` → `.substr($i, 1)` | 10000, 10.65s, 3.81 | 100000, 0.26s, 1.75 |
| `"a" x N` → `.substr($i, 1)` | 20000, 2.66s, 3.74 | 200000, 0.28s, 2.01 |
| `.index("あ", $i)` | 10000, 23.15s, 4.12 | 100000, 0.89s, 1.89 |
| `.rindex("あ", $i)` | 10000, 22.36s, 4.01 | 100000, 0.92s, 2.11 |
| `.contains("あ", $i)` | 10000, 0.95s, 4.00 | 100000, 0.24s, 1.66 |
| `.substr-eq("あ", $i)` | 20000, 0.61s, 3.62 | 200000, 0.52s, 1.93 |
| `.starts-with("あ")` | 50000, 0.96s, 3.57 | 500000, 0.52s, 1.95 |
| `.ord` | 50000, 0.93s, 3.60 | 500000, 0.49s, 1.98 |
| `.indices("a")` (one call) | 10000, 0.73s, 4.00 | 100000, 0.02s, 1.82 |

Every row is now linear in the total work. (rakudo runs the `.substr` row at
N=200000 in 0.06s on the same box, so a constant factor remains — that is the
interpreter's per-call cost, not this path.) Pinned by
`t/types/string/str-positional-grapheme-index.t` and the unit tests in
`grapheme_index.rs`, which check every conversion against the old
`grapheme_units` segmentation.
