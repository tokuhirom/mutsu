# sprintf copies UTF-8 format literals as UTF-8, not Latin-1

`printf "√2 ≈%.9f\n", $x` wrote mojibake (`â2 â1.414…`, `Ï` for `π`) because
`format_sprintf_impl` walked the format string a byte at a time and pushed each
byte as a Latin-1 codepoint:

```
√  U+221A  UTF-8 E2 88 9A  →  U+00E2 U+0088 U+009A  →  UTF-8 C3 A2 C2 88 C2 9A
```

A UTF-8 terminal then shows `â` and swallows the C1 continuation bytes, which
is exactly the first-run output of `contfrac.raku`. `say "√2 ≈ π"` was already
correct — only sprintf/printf format *literals* were wrong; `%s` arguments
were fine.

The formatter now copies each run of literal text as a UTF-8 slice up to the
next ASCII `%`. Directive letters are consumed as a whole Unicode scalar so
the scan stays on a character boundary.

Pinned by `t/sprintf-utf8-literal.t`.
