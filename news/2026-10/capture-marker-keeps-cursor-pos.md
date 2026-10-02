# A `)>` capture marker no longer moves `.pos` or fails `Grammar.parse`

A `<(` / `)>` marker narrows a Match's `.from`/`.to`, but in Raku it does not
move `.pos`, the position the cursor reached:
`'<abc>' ~~ /'<' <( \w+ )> '>'/` has `.to` 4 and `.pos` 5. mutsu treated the
two as the same value. That reported the wrong `:pos` in `.raku`, and it made
`Grammar.parse` fail whenever the start rule ended in narrowed context
(`token TOP { '<' <( \w+ )> '>' }`, or a marker inside a `~` goal match's
inner pattern). The parse compared the narrowed `.to` against the subject
length instead of checking where the cursor ended (#10570).

Every match entry point now settles its span through one
`RegexCaptures::finish_span(start, end)`. It applies the markers and, only
when one fired, keeps the cursor's own span on the accumulator's cold
payload. `Grammar.parse`'s anchoring and full-coverage checks read that
cursor span. The built Match carries the cursor end as `.pos` (stored on
the capture node's child payload, and only when it differs from `.to`).
`.pos`, `nqp::getattr(.., '$!pos')` and `.raku` report it. A match without a
marker is unchanged and allocates nothing extra.
