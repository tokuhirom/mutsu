# `.comb.head(3)`, `.lines.head(3)` and `$fh.lines.head(3)` read only a prefix

Closes #9251.

`Str.comb` with no matcher cut the whole string into graphemes and built one `Str` per grapheme
before `.head` looked at any of them. For a 1M-char string, 20 calls of `$s.comb.head(3)` took
6.7 s, and 13.5 s at 2M. Rakudo takes 3 ms at either size. `$fh.lines.head(3)` on a 4 MB file
was worse: 1.5 s per call, and the handle was left at EOF.

## What rakudo does

Disassembling the setting (`moar --dump CORE.c.setting.moarvm`) shows that `Str.comb` is
`Seq.new(<iterator>.new(self))`. The iterator holds `$!str` and a `$!pos` cursor, and its
`pull-one` is one `substr`. `comb`, `comb(Int)`, `comb(Str)`, `lines`, `words` and `split` each
have an iterator class of this shape. `.head(n)` pulls `n` times. `count-only` counts without
building strings, and `sink-all` moves the cursor to the end.

## What mutsu does now

The same cursor lives in `src/value/str_iter.rs` (`StrIterSpec`). The Seq that `comb` (no
matcher, `Int`, `Str`), `lines` and `words` return holds it as a deferred source
(`SeqSource::StrIter`). This covers the method and sub forms, with and without `$limit`.
Building that Seq copies nothing. [ADR-0119](../../docs/adr/0119-seq-sources-pulled-a-prefix-at-a-time.md)
records the design:

- The first read of the Seq settles the cursor in place. The cursor needs no interpreter, so
  `SeqBody::deref` can do it, and from then on the Seq is exactly the eager one mutsu built
  before. Consumption rules, itemization, `.raku`, `.Set`, `~~` and full-consumption speed are
  unchanged: a consumer matrix of about 60 operations over 12 forms gave byte-identical output before and after.
- A consuming `.head(n)` / `.head` / `.first` steals the source and pulls only `n` elements. An
  `IO::Handle.lines` / `.words` read (`SeqSource::IoLines`) is served the same way. In rakudo
  these calls consume the Seq, so no partly-read state is kept.
- A bounded subscript (`$s.comb[0]`, `$s.lines[^2]`) goes through `SeqBody::pull_prefix`. That
  is the old `IoLines`-only prefix read, generalized.
- `.lines(:count)` counts with the cursor and builds no lines.

A first prototype put the cursor into `LazyList` instead, like `IO::CatHandle.lines`. It was
just as fast for `.head`, but it broke `X::Seq::Consumed`, the `$( )` of an itemized Seq, `.Set`
and `~~`. Each of those rules lives on `SeqBody` and would have needed a second copy. The ADR
records that comparison.

## Measured (release build)

| case | before | after | rakudo |
| --- | ---: | ---: | ---: |
| `$s.comb.head(3)` ×20, 1M / 2M chars | 4.41 s / 9.95 s | 0.0004 s / 0.0002 s | 0.0024 s / 0.0003 s |
| `$fh.lines.head(3)` ×5, 4 MB / 8 MB | 7.43 s / 14.55 s | 0.0003 s / 0.0003 s | 0.040 s / 0.022 s |
| `$fh.words.head(3)` ×5, 4 MB / 8 MB | 8.75 s / 18.05 s | 0.0001 s / 0.0002 s | 0.049 s / 0.041 s |

`scripts/str-complexity-check.sh` has six new cases (`comb.head(3)`, `comb(2).first`,
`comb(Str).head(3)`, `comb[1]`, `lines.head(3)`, `words.head(3)`). Each loops NN calls over an
NN-sized string. The previous build shows ratio 3.8–3.95 (SUPERLINEAR), and this build shows ~2.

After `$fh.lines.head(2)`, `$fh.get` now returns the third line, as it does in rakudo. Before, it
returned `Nil`.

## Not covered

- `IO::Path.lines` / `.words` slurp the whole file (#9257).
- `comb(Regex)` finds every match up front.
- `split` is O(n) for `.head` in rakudo too.

Tests: `t/collections/lazy-seq/str-cursor-seq-prefix.t` and `t/io/handle-lines-head-reads-prefix.t`.
Both pass on mutsu and raku.
