# The streaming `Encoding::Decoder` honours the encoding it was built with

`Encoding::Registry.find('iso-8859-1').decoder()` decoded its buffer as UTF-8.
Every byte above 0x7F therefore came back as U+FFFD:

```raku
my $d = Encoding::Registry.find('iso-8859-1').decoder();
$d.add-bytes(Buf.new(0xE1, 0xE2, 0xB5));
say $d.consume-all-chars();   # raku: "áâµ"    mutsu: "\x[FFFD]\x[FFFD]"
```

`Buf.decode('latin-1')` was always right — the gap was only in the *streaming*
decoder, whose `decode_bytes` special-cased `utf8-c8` and otherwise ran
`String::from_utf8_lossy` regardless of the `encoding` attribute it had stored.
`consume-all-chars`, `consume-available-chars` and `consume-line-chars` all went
through it.

## Fix

`builtins` exposes `decode_bytes_with_encoding_label`, which resolves a label
through the same alias table `.decode` uses (`latin-1` / `latin1` /
`iso-8859-1` are one encoding) and decodes with the existing per-encoding
decoder. `decode_bytes` now consults it first and only falls back to the lossy
UTF-8 read on a decode error, which is what a streaming decoder needs for an
incomplete trailing sequence.

`decode_available` gained a companion gate, `is_single_byte_encoding_label`: for
a single-byte encoding the whole buffer is always complete, so it is handed back
in one piece instead of running UTF-8's "back off to the last valid sequence"
walk (which would have discarded every high byte as an incomplete sequence).

Pinned by `t/streaming-decoder-encoding.t`.

## Effect

Cro builds its HTTP header decoder exactly this way, so any header value outside
ASCII was mangled. `t/http-request-parser.rakutest`'s "Field value can be any
printable char including latin-1 range" now passes.
