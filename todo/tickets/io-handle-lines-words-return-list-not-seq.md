# `IO::Handle.lines` / `.words` return a `List`, not a `Seq`

`IO::Handle.lines` and `IO::Handle.words` are specified to return a `Seq` (see
`raku-doc/doc/Type/IO/Handle.rakudoc`), like every other lazy line/word reader.
mutsu returns an eager `List` from the handle forms. The `IO::Path` forms are
correct — `"f".IO.lines.WHAT` is `Seq` — so the divergence is only on an open
handle.

## Repro

```raku
my $fh = 'some-file'.IO.open;
say $fh.lines.WHAT;   # raku: (Seq)   mutsu: (List)
say $fh.words.WHAT;   # raku: (Seq)   mutsu: (List)
$fh.close;
```

Visible in `.raku` output (`("a", "b").Seq` vs `("a", "b")`) and to anything that
type-checks the result or relies on one-shot consumption.

## Why it is not a one-liner

The return type is the shallow half. `Seq` is *lazy and one-shot*: reading it
should pull from the handle on demand, and re-consuming it should throw
`X::Seq::Consumed`. Wrapping the already-eager `Vec` in a `Seq` would fix the
type check while leaving the laziness wrong — a partially consumed handle
followed by `.lines` must yield only the remainder, and an infinite/streaming
source (a socket, a pipe) must not be drained up front. Doing this properly means
routing the handle read through the iterator machinery, which is why it is filed
rather than fixed in place.

## How it was found

Comparing every text-read path against raku while implementing the CRLF
decode translation (`news/2026-07/text-reads-translate-crlf.md`). It was the only
residual difference in that sweep and is unrelated to line endings.
