# `:st` / `:nd` / `:rd` / `:th` are exact aliases of `:nth`

The ticket described `:st(positions)` as an adverb that "restricts matching to
only start at one of the given position(s)". It is not a positional adverb at
all. `Language/regexes.rakudoc:2679` says it plainly:

> There's actually no difference between the `:nth` adverb and the rest. You
> choose them only based on legibility.

Measured against `raku` v2026.06 on `"f fo foo fooo foooo fooooo foooooo"`,
`m:st(2)/fo+/`, `m:nd(2)/fo+/`, `m:rd(2)/fo+/`, `m:th(2)/fo+/` and
`m:nth(2)/fo+/` all give `｢foo｣` — the *second match*, not a start offset.

mutsu already implemented the whole ordinal family, including the `Junction`
and list argument forms (`:nth(1|8)`, `:nth(1,3)`). The gap was one branch in
the adverb parser: `st` / `nd` / `rd` picked up an ordinal only from the
*digit-prefix* spelling (`:1st`, `:2nd`, `:3rd`), and `th` was the only one of
the four that also accepted a parenthesised argument. `:st(1|8)` therefore fell
off the end of the `if` chain into "Unsupported regex adverb :st".

The four names now share one arm with `nth`, taking their ordinal from the
digit prefix when there is one and from the parenthesised argument otherwise.
They were also added to the two lists that were already spelled as
`"nth" | "th" | "x"`: the `first_match_adverb` record (so `rx:st(1)/a/` names
the offending adverb in its "not allowed on rx" error, as Rakudo does) and the
square-bracket rejection (`m:st[5]/…` is the same mistake as `m:nth[5]/…`).

Pinned by `t/regex-engine-gaps.t`.
