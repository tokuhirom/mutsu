# String literals in the source are NFC-normalized

Raku's `Str` is NFG, so a literal written in the program text is normalized when
the program is compiled. mutsu already normalized the buffer around an *escape*
(`\x[2126]`, `\x[0041,0300]` — `parser/primary/string/escapes.rs`) and, since
`news/2026-08/decoded-strings-are-nfc.md`, `.decode` output. Raw non-ASCII text
in the source went through untouched:

```raku
my $lit = 'Ω';                    # written as U+2126 OHM SIGN
say $lit.encode('utf-8').elems;    # raku: 2      mutsu: 3
say $lit eq "\x[03A9]";            # raku: True   mutsu: False
```

## Fix

`literal_str` in `parser/primary/string/helpers.rs` builds a string-literal
`Value` through the shared `builtins::nfc` (the same helper the decode path
uses, with its `is_nfc_quick` gate, so ASCII literals are free). Every
`Expr::Literal(Value::str(…))` construction in the quoting constructs —
single/double quotes, `q`/`qq`, heredocs, word quoting, `qx`, and the
interpolation part-builders — goes through it.

Pinned by `t/source-literals-are-nfc.t`. Its literals are written with U+2126 on
purpose; the file says so, since an editor "tidying" them would silently make
the test vacuous.

## Effect

Found while measuring the Cro suites: `t/http-request-parser.rakutest` failed
exactly one of 344 checks,

```raku
*.query-value('ΩΩ') eqv '2omega';
```

and the test file really does spell `ΩΩ` with U+2126, while the value comes from
percent-decoding `%E2%84%A6%E2%84%A6` — normalized since the decode fix, so the
two no longer matched. **Cro's `t/http-request-parser.rakutest` is now
344/344.**
