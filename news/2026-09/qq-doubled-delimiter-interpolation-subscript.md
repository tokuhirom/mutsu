# `qq[[@a[0]]]` parses: the close scan steps over an interpolation atom

An interpolated variable whose subscript ended exactly where a doubled
delimiter's closing run begins was mis-scanned into a parse failure:

```
$ raku  -e 'my @a = 1,2; say qq[[@a[0]]]'
1
$ mutsu -e 'my @a = 1,2; say qq[[@a[0]]]'      # before
===SORRY!=== Error while compiling -e
```

## The phase split

mutsu parses a quote in two phases: `parse_q_quoted_content` first finds the
closing delimiter with a purely textual scan — `read_multi_bracketed` for a
repeated delimiter — and only then hands the extracted content to
`interpolate_string_content`. In `qq[[@a[0]]]` the text after the `[[` opener is
`@a[0]]]`, whose first `]]` sits immediately after the `0`. The scan stopped
there, yielding the content `@a[0` (an unterminated subscript) and leaving a
stray `]` behind.

Rakudo has no such split: its quote grammar parses the interpolation atom
`@a[0]` as a unit — subscript included — and only then looks for the close. That
is also why rakudo's behaviour here is *not* single-bracket nesting:
`qq[[a[b]]]` is a syntax error in rakudo (no sigil, so nothing consumes the
`[b]`), while `qq[[$x[0]]]` and `qq[[{ 1+1 }]]` both work. Counting bare
brackets would have made `qq[[a[b]]]` wrongly succeed.

## Measuring the atom instead of re-deriving it

The scan now steps over a whole interpolation atom when the quote interpolates
and the delimiter is repeated. The obvious way to write that skipper — sigil,
name, then a hand-rolled chain of balanced postfixes — is exactly what the
ticket warned against: it has to agree with what `interpolate_string_content`
later consumes, or the two disagree and the string silently changes.

So it does not re-derive the rule. `interpolation_atom_end` calls
`try_interpolate_var` — the very function that will later consume the atom —
with a throwaway output buffer and reads back how much input it took. Agreement
is then true by construction, including the parts that are easy to get subtly
wrong: `@a[0]` scans to the FIRST `]` (not a balanced one), `%h<k>` to the first
`>`, `%h{…}` balanced, and no postfix may be preceded by whitespace.

The extra parse is paid only for a *repeated* delimiter in an *interpolating*
quote, which is where the phase split is observable at all. Both entry points
thread the flag: the direct `qq[[…]]` form and the adverb-driven one
(`q:qq[[…]]`, via `read_delimited_content`), which had the same bug.

## Scope

The architecturally right answer is still to stop scanning for the close ahead
of parsing the content — to let the quote parser find the close as rakudo's
grammar does — but that is a larger change to the quote slang. This fix removes
the observable divergence without introducing a second, divergeable copy of the
interpolation rules.

Pinned by `t/quote-doubled-delimiter-interpolation.t`, whose 19 assertions were
measured against rakudo 2026.07 and pass identically under both. The existing
`t/quote-doubled-delimiter.t` (25 assertions, including the two syntax errors
that must stay errors) is unchanged and still passes.
