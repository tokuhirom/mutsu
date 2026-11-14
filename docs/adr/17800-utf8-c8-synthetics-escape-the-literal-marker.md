# ADR-17800: A utf8-c8 synthetic stays in-band, and a literal U+10FFFD is escaped

- **Status**: Proposed
- **Date**: 2026-11-14
- **Deciders**: tokuhirom, Claude
- **Issue**: [#17800](https://github.com/tokuhirom/mutsu/issues/17800)
- **Related**: #15931 (the grapheme segmenter's utf8-c8 rule), `src/runtime/utf8_c8.rs`,
  `src/grapheme_break.rs`, ADR-0120 (the `StrBody` strand representation)

## 1. Context

Rakudo's `utf8-c8` decoder turns each byte that is not part of valid UTF-8 into one NFG
synthetic grapheme. The synthetic's codepoints, as `.ords`, `.NFD` and `.encode('utf8')`
expose them, are `U+10FFFD 'x' H H`, with `HH` the byte in uppercase hex. The synthetic itself
is a distinct grapheme that only the utf8-c8 decoder creates. Ordinary text that happens to
contain those four codepoints is four graphemes and is not equal to the synthetic:

```raku
my $s = Blob.new(255).decode("utf8-c8");
say $s.encode("utf8").decode("utf8") eq $s;   # False
say "\x[10FFFD]xFF".chars;                     # 4
```

mutsu stores a `Str` as Rust UTF-8 (`StrBody`, which derefs to `&str`). It represents the
synthetic as the same four codepoints, and `grapheme_break` treats `U+10FFFD x H H` as one
grapheme *wherever it occurs*. The synthetic and the literal text are therefore the same value:
mutsu answers `True` and `1` above. LibTmux detects bytes that are not Unicode with exactly
that round trip (`text-value` in `LibTmux::Query`), so `t/unit/query.rakutest` differs from
rakudo.

A Rust `String` can hold every Unicode scalar value, so no codepoint sequence is free for an
in-band marker that ordinary text could never contain. One of the two spellings has to change.

## 2. Decision

Keep the synthetic's in-band form, `U+10FFFD 'x' H H`. Store a **literal** `U+10FFFD` as the
doubled marker `U+10FFFD U+10FFFD`. Inside a `Str`, a `U+10FFFD` is then always the first
codepoint of one of two forms:

| internal form | meaning | graphemes | codepoints exposed |
| --- | --- | --- | --- |
| `U+10FFFD 'x' H H` | utf8-c8 synthetic for byte `HH` | 1 | `U+10FFFD x H H` |
| `U+10FFFD U+10FFFD` | the literal character `U+10FFFD` | 1 (plus any marks) | `U+10FFFD` |

A lone `U+10FFFD` that is followed by neither form does not occur in a well-formed `Str`. The
segmenter and every exposing boundary treat it as a literal if one ever does.

The escape is applied at every boundary where codepoints enter a `Str` and undone at every
boundary where codepoints leave one. There is one helper pair for each, beside
`decode_utf8_c8`/`encode_utf8_c8` in `runtime::utf8_c8`:

- **In** (`escape_literal_marker`, a no-op without `U+10FFFD`):
  - every decoder: `Blob.decode`, handle reads, `lines`, `slurp`, the stream decoder,
    utf8-c8's valid-sequence path, and the other encodings;
  - `chr`/`chrs`, `Uni.Str`/`.NFC`/`.NFD`→`Str`, `\x[...]`/`\c[...]` escapes, and source
    text, including string literals and heredocs;
  - `nqp::chr` and the `nqp::` codepoint constructors.
- **Out** (`unescape_literal_marker`):
  - every encoder, and writes to handles and sockets;
  - `.ords`, `.ord`, `.NFC`/`.NFD`/`.NFKC`/`.NFKD`, `.uniname`/`.uniprop` of the literal;
  - `nqp::ord`/`nqp::codes`.
- **Grapheme segmentation**: `U+10FFFD U+10FFFD` is one grapheme base that combining marks may
  extend, exactly like any other base character. `U+10FFFD x H H` stays the closed synthetic
  cluster it is today.
- **Everything else**: comparison, hashing, `index`, `substr`, regex and concatenation works
  on the internal form unchanged. Both forms start at a grapheme boundary and are internally
  unambiguous, so byte-wise equality of the internal form is grapheme equality. The literal
  and the synthetic compare unequal, as in rakudo.

## 3. Consequences

- `Blob.new(255).decode("utf8-c8").encode("utf8").decode("utf8") eq` the original is `False`.
  `"\x[10FFFD]xFF".chars` is 4. LibTmux's query file matches rakudo.
- The common case pays nothing: a string with no `U+10FFFD` takes the existing fast paths. Each
  boundary is a `memchr`-style scan for the marker's UTF-8 lead bytes (`F4 8F BF BD`), which is
  skipped when the input is ASCII-only, as most decoder fast paths already know.
- `str.len()` in bytes no longer equals the encoded UTF-8 length for a string containing a
  literal `U+10FFFD`. Code that sizes an encode buffer from `len()` must go through the encoder.
  This is rare, and the encoders own it.
- A missed **In** boundary would let a literal marker through un-escaped. It then reads as a
  synthetic only when followed by `x H H`, which is exactly today's behaviour, so a missed
  boundary is a residual of the present bug, never a new one.
- A missed **Out** boundary would show a doubled `U+10FFFD`. A property test is kept to catch
  that: random strings with `U+10FFFD` sprinkled in, round-tripped through every encoder and
  `.ords`/`chrs`.

## 4. Alternatives considered

- **An out-of-band synthetic table** (positions of synthetics carried beside the `StrBody`,
  MoarVM-style negative "codepoints"). This is exact, but every producer of a `Str` (concat,
  `substr`, `x`, regex captures, `trans`, `subst`, strands) would have to carry and re-base the
  table, and any producer that drops it silently turns synthetics into literals. That is far
  more surface than the boundaries above, and it competes with ADR-0120's strand
  representation.
- **A private grapheme store like MoarVM's NFG** (a `Str` of grapheme ids). This is the
  long-term NFG answer, but it is a whole-runtime string rewrite and out of proportion to this
  bug.
- **Escape the synthetic instead** (give it a new spelling and keep literals raw). The
  synthetic's spelling is observable (`.ords`, `.encode`, `.NFD`), so it would need the same
  boundary translation in the opposite direction. Synthetics are produced in one place (the
  utf8-c8 decoder) but *exposed* in as many places as literals, so nothing is saved.
  Escaping the rarer literal keeps the synthetic, which mutsu already produces and tests,
  untouched.
- **Do nothing**: leaves LibTmux and any program that validates text this way differing from
  rakudo.

## 5. Open questions

- Source text: does any `use`d module or EVAL'd string in the ecosystem contain a literal
  `U+10FFFD`? None was found in the vendored trees. The parser boundary is still required for
  correctness.
- Should `Str.chars` keep the existing grapheme-count cache keyed on the internal form? Yes,
  if counting treats `U+10FFFD U+10FFFD` as one grapheme. This is to be confirmed when
  implementing.
