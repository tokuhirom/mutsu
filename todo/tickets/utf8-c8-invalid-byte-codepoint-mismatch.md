# `utf8-c8` decoding of an invalid byte uses a different private-use codepoint than raku

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/unicode.rakudoc:83` and `:90`).

## Root cause hypothesis

The `utf8-c8` encoding is a lossy-roundtrip variant of UTF-8: when decoding a byte sequence
that isn't valid UTF-8, instead of using the standard U+FFFD replacement character (which
loses the original byte value), Rakudo synthesizes a private-use-area codepoint that
records the original invalid byte, so it can later re-encode back to the exact original
bytes. mutsu also produces a private-use codepoint for an invalid byte (so its `utf8-c8`
decoding is at least attempting the right feature) but picks a **different** synthetic
codepoint than raku's scheme, so the visible output (and any downstream `.ords`) differs.

## Minimal repro

```raku
say Buf.new(ord('A'), 0xFE, ord('Z')).decode('utf8-c8');
```

- `raku`: `A􏿽xFEZ` (the invalid byte `0xFE` becomes a specific synthetic PUA codepoint
  that raku's terminal rendering shows as `􏿽xFE` — U+10FFFD followed by the literal hex
  digits, i.e. Rakudo's synthetic-codepoint scheme stringifies to show the original byte's
  hex value as plain text after a fixed marker character)
- `mutsu`: `A󰃾Z` — a different-looking synthetic character, not matching raku's
  hex-suffixed rendering.

Same root cause, via `slurp(..., enc => 'utf8-c8')` after writing raw bytes to a file:

```raku
my $test-file = "/tmp/test";
given open($test-file, :w, :bin) {
  .write: Buf.new(ord('A'), 0xFA, ord('B'), 0xFB, 0xFC, ord('C'), 0xFD);
  .close;
}
say slurp($test-file, enc => 'utf8-c8');
```

- `raku`: `A􏿽xFAB􏿽xFB􏿽xFCC􏿽xFD` (each invalid byte gets the same hex-suffixed rendering)
- `mutsu`: `A󰃺B󰃻󰃼C󰃽` (different synthetic codepoints per byte)

(The harness bucketed the second repro as `raku-drift-from-doc` due to a doc `# OUTPUT:`
annotation quirk, but the underlying divergence from `raku`'s actual output is real and
shares the exact same root cause as the first repro.)

## Affected files (starting point)

- Wherever `utf8-c8` decoding is implemented (grep `utf8-c8` in `src/builtins/` /
  `src/runtime/`) — needs to match Rakudo's exact synthetic-codepoint scheme for invalid
  bytes (a fixed high codepoint whose low bits or accompanying rendering encode the
  original byte, matching the `〈marker〉xNN` stringification raku shows), not an
  independently-chosen PUA codepoint.
