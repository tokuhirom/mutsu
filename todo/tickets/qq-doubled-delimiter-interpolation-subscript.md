# `qq[[@a[0]]]` fails to parse: the close scan runs before interpolation

An interpolated variable whose subscript ends exactly at a doubled quote
delimiter's closing run is mis-scanned:

```
$ raku  -e 'my @a = 1,2; say qq[[@a[0]]]'
1
$ mutsu -e 'my @a = 1,2; say qq[[@a[0]]]'
===SORRY!=== Error while compiling -e
```

Measured 2026-09-05 on `main` at `2a9e06f91`.

## Why

mutsu parses a quote in two phases: `parse_q_quoted_content`
(`src/parser/primary/string/q_string.rs`) first finds the closing delimiter with a
purely textual scan — `read_multi_bracketed`
(`src/parser/primary/string/helpers.rs`) for a repeated delimiter — and only then
hands the extracted content to `interpolate_string_content`. In `qq[[@a[0]]]` the
text after the `[[` opener is `@a[0]]]`, whose first `]]` sits immediately after
the `0`. The scan stops there, yielding the content `@a[0` (an unterminated
subscript) and leaving a stray `]` behind, so the parse fails.

Rakudo has no such phase split: its quote grammar parses the interpolation atom
`@a[0]` as a unit — subscript included — and only then looks for the close. That
is also why rakudo's behaviour here is *not* single-bracket nesting: `qq[[a[b]]]`
is a syntax error in rakudo (no sigil, so nothing to consume the `[b]`), while
`qq[[$x[0]]]` and `qq[[{ 1+1 }]]` both work. Any fix has to reproduce that
distinction — counting single brackets would make `qq[[a[b]]]` wrongly succeed.

## Scope

Narrow. The bug needs all three of: a repeated delimiter, interpolation, and a
subscript (or other bracketed postfix) whose closing bracket abuts the
delimiter's closing run. Every neighbour is already correct and pinned by
`t/quote-doubled-delimiter.t`:

```
qq[@a[0]]        # ok -- single delimiter, incidental bracket nesting covers it
qq[[x@a[0]y]]    # ok -- the subscript does not abut the close
qq{{@a[0]}}      # ok -- `}` is not the subscript's bracket
qq[[%h<a>]]      # ok
```

## Where to look

`read_multi_bracketed` would need to skip an interpolation atom (sigil, name,
then a chain of balanced `[...]` / `{...}` / `<...>` / `(...)` postfixes) when
scanning an *interpolating* quote, which means passing an `is_qq` flag down from
`parse_q_quoted_content`. That skipper must agree with what
`interpolate_string_content` later consumes, or the two disagree and the string
silently changes — which is why this is a ticket and not a one-liner. The
architecturally right answer is to stop scanning for the close ahead of parsing
the content at all, and let the quote parser find the close as rakudo's grammar
does; that is a larger change to the quote slang.

## Repro

The four lines under "Scope", plus the headline. No fixtures.
