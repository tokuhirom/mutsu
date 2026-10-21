# Deferred map/grep prefix pulls share one chunk step

The prefix pull of a deferred `.map`/`.grep` and the pull of a map chained onto another
one each carried their own copy of "run one chunk, advance `pos` past what was
consumed (also when the callback dies), and detect a `last` that ended the Seq".
Both now call `pull_chunk_step`, so the `last`-depth bookkeeping and the
`pos`/`consumed` accounting exist once (#14196). Behaviour is unchanged.

A single loop with a produced-element limit (the issue's full proposal) stays open:
the map loops take their source as a `Vec` slice, so a limit over `pos..len` would
copy the whole remaining source on every prefix pull. It needs the map loops to read
through a fetch feed, as the grep loop already does (`GrepFeed`).
