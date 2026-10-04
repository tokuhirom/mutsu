# Str's comb, words, lines and ords are method-table rows

The zero-argument string iteration methods `comb`, `words`, `lines` and
`ords` are now rows in the built-in method table (ADR-11276). Each has a row
owned by `Str` and one owned by `Cool`, and both point at the same handler,
so `12345.comb` reaches the same implementation as `"12345".comb`.

The answers are unchanged. `comb`, `words` and `lines` still return a lazy
Seq over the receiver's string form, and `ords` still returns a Seq of its NFC
codepoints. A call on a variable now goes through the call-site lane.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$s.comb` | 1,971M | 931M | -52.8% |
| `$s.words` | 1,973M | 931M | -52.8% |
| `$s.lines` | 1,948M | 931M | -52.2% |
| `$s.ords` | 2,367M | 1,230M | -48.1% |
| `$i.comb` (Int) | 1,731M | 993M | -42.7% |
| `$s.flip` (control) | 472M | 472M | 0.0% |
