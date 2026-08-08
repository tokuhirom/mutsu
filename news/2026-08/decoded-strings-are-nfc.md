# A decoded string is NFC-normalized, like every other Raku string

Raku's `Str` is NFG, so a string built from bytes is normalized at creation.
mutsu normalized string *literals* at parse time
(`parser/primary/string/escapes.rs`) but never normalized `.decode` output, so a
decoded string compared unequal to an identical literal:

```raku
my $s = Buf.new(0xE2, 0x84, 0xA6).decode('utf-8');   # U+2126 OHM SIGN
say $s.ords;                     # (937)     -- looks composed
say $s.encode('utf-8').elems;    # raku: 2   mutsu: 3
say $s eq "\x[03A9]";            # raku: True  mutsu: False
```

`.ords` normalizes on read, which is what made the bug so hard to see: every
diagnostic printed the same code points while `eq`, `cmp`, `.encode` and hash
lookup all disagreed.

## Fix

`decode_bytes_with_builtin_encoding` NFC-normalizes its result, via a quick
check (`is_nfc_quick`) that returns the string untouched — and allocation-free —
for the overwhelmingly common already-normalized case. `utf8-c8` is exempt:
keeping invalid bytes as synthetic code points is the whole point of that
encoding, and its round-trip must be exact.

Pinned by `t/decoded-strings-are-nfc.t`.

## Effect

Found while closing out Cro's `t/http-request-parser.rakutest`. Cro
percent-decodes a query string by turning `%XX` escapes into bytes and decoding
them, so `?%E2%84%A6%E2%84%A6=2omega` produced a key that *printed* as `ΩΩ` but
that no lookup with a literal `ΩΩ` could find:

```raku
*.query-hash eqv { 'a/b' => '2 3', love => '♥', ΩΩ => '2omega' },   # was False
*.query-value('ΩΩ') eqv '2omega';                                    # was Any
```

That was the file's last remaining failure; it is now fully green under mutsu.
