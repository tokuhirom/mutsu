# `utf8-c8` renders an invalid byte with Raku's synthetic marker codepoint

Decoding a byte that is not valid UTF-8 under the `utf8-c8` encoding must keep
it as a single grapheme built from the synthetic marker codepoint `0x10FFFD`
followed by an uppercase hex rendering of the byte, so that re-encoding
round-trips to the original bytes:

```raku
Buf.new(ord('A'), 0xFE, ord('Z')).decode('utf8-c8').ords;
# (65 1114109 120 70 69 90)
```

The ticket filed for a codepoint mismatch here was stale — `1c1098c29` ("fix:
match raku utf8-c8 invalid byte codepoints") had already brought
`src/runtime/utf8_c8.rs` and its callers in line, pinned by
`t/utf8-c8-invalid-byte-codepoint.t` (which covers grapheme count, the marker
codepoint, the hex payload, the surrounding valid codepoints, the round-trip,
and a multi-invalid-byte case). Re-running the ticket's repro verbatim against
current `main` gives ord-for-ord identical output under `raku` and `mutsu`. The
ticket was never removed when the fix landed; this closes the bookkeeping.
