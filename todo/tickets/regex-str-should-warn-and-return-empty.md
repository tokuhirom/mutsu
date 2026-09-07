# `Regex.Str` returns the source text instead of warning and returning `""`

Found 2026-09-07 while fixing
`news/2026-09/smartmatch-against-a-bare-topic-rhs.md`, where it was the one row
in the measurement matrix that still diverged after the fix. Independent of
that change: it is a `Regex` coercion question, not a smartmatch one.

## Repro

```raku
say (/a/).Str.raku;
# raku:  Regex object coerced to string (please use .gist or .raku to do that)
#        ""
# mutsu: "/a/"
```

Two things differ: raku emits a warning, and it yields the **empty string**
rather than the regex's source text.

## How it surfaces

Anything that smartmatches a `Regex` against a `Regex`, which is what a
self-match of a regex-valued topic does:

```raku
for (/a/,) { say ($_ ~~ $_).raku }
# raku:  Nil     (plus the coercion warning)
# mutsu: Match.new(:orig("/a/"), :from(1), :pos(2))
```

The LHS `/a/` is stringified so the RHS regex has a subject. raku's `""` cannot
contain `a`, so the match fails and the smartmatch is `Nil`; mutsu stringifies
to `"/a/"`, which *does* contain an `a` at offset 1, so it reports a spurious
`Match`. The same applies to `my $r = /a/; $r ~~ $r`, and to any `~` /
interpolation of a bare `Regex`.

## Where to look

The `Str` coercion for `ValueView::Regex` — grep for where a `Regex` is turned
into a display string and separate the two consumers, which currently share
one answer: `.gist` / `.raku` (which legitimately show the source text and must
keep doing so) and `.Str` / string context (which must warn and return `""`).

## Check when fixing

`.gist` and `.raku` of a `Regex` are unchanged; `"{/a/}"` and `"" ~ /a/` warn
and interpolate nothing; the warning goes to `stderr` with the same wording and
is suppressible by `quietly`; `Regex ~~ Regex` becomes `Nil`; and
`t/smartmatch-bare-topic-rhs.t` still passes.
