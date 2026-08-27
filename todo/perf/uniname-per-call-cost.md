# `.uniname` costs ~7x raku per call

Split out of `todo/perf/uniname-sort-performance-gap.md` when the two larger
findings in it were fixed (see
`news/2026-08/native-array-map-loop-was-9x-slower-than-the-shared-loop.md` and
`news/2026-08/sort-key-extraction-batched-through-the-map-loop.md`). This is the
small residue: a per-call cost in `.uniname` itself, independent of the
call-machinery overhead that dominated the original repro.

## Measured (release, 131072 iterations of a plain `for` loop)

```raku
my @cps = (0..0x1FFFF).List;
my $t = now; my $s = 0; for @cps { $s += $_.uniname.chars }; say +(now - $t);
```

| | mutsu | raku |
|---|---|---|
| `for @cps { $s += $_.uniname.chars }` | 0.247s | 0.042s |
| `for @cps { my $x = $_ + 1 }` (loop floor) | 0.058s | 0.015s |

So ~1.44 us of actual `.uniname` + `.chars` work per call against raku's ~0.2 us.
It shows up in a bare `for` loop, so it is *not* the closure-call machinery — it
is the name lookup and/or the method dispatch around it.

## Where to look

`builtins/unicode.rs::unicode_char_name_by_codepoint` allocates a fresh `String`
on every call: `unicode_names2::name(ch)` returns an iterator-shaped `Name` that
is materialized with `.to_string()`, and every sentinel/derived arm goes through
`format!`. `.chars` then only needs the *length*, so for this shape the whole
allocation is wasted.

Worth profiling before assuming a cause — measure whether the cost is in
`unicode_names2`'s lookup, in the allocation, or in the `.uniname` method
dispatch itself (compare against another 0-arg `Str`-returning method on an Int).
Candidate directions, in rough order of expected payoff:

1. Return `Cow<'static, str>` (or expose a `uniname_len`) so the common
   `unicode_names2` hit does not allocate.
2. Cache nothing — a per-codepoint cache would be a large table for a rarely
   hot method; only consider it if profiling says the lookup, not the
   allocation, dominates.

## Priority

Low. `.uniname` is not on any hot path we know of; this was found by a
synthetic sweep over the entire Unicode range. Recorded so the measurement is
not lost, not because it blocks anything.
