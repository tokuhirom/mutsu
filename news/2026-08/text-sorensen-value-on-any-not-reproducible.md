# `Text::Sorensen`'s `t/01-basic.t` failure no longer reproduces

`todo/tickets/dist-test-suite-failures-batch.md` tracked a triaged failure in
the `Text::Sorensen` dist's own test suite: `No such method 'value' for
invocant of type 'Any'` at `t/01-basic.t:15`, reached after subtest 3 of 21
(found by the 2026-07-25 `--run-tests` sweep).

Re-running the dist's test suite against current `main`
(`raku -I lib t/01-basic.t` under mutsu, from the cached sweep tarball at
`~/.cache/mutsu-dist-sweep/T_EX_TEXT_SORENSEN_*.tar.gz`) now passes all 21
subtests cleanly, matching `raku` exactly. The suspect code path — the
`%hash.race.map({ ... .value ... })` multi candidate of `sorensen()`/`jaccard()`
in `lib/Text/Sorensen.pm6`, which calls `.value`/`.key` on the topic during a
`Bag`/`Bag` set intersection and symmetric-difference calculation — was
reduced to a minimal standalone repro and also passes.

No code change; the underlying fix landed as a side effect of unrelated work
between 2026-07-25 and 2026-08-06. Closing the ticket item.
