# `done` in a `whenever` completes its block even during the source's setup

A `supply` block that `done`s inside a `whenever` lost that `done` when the
value had arrived while the block was still opening the source: a source
supply block that emits during its own body (`supply { emit 3; whenever
$live { } }`) delivers synchronously, before the block had wired up the
marker that turns `$emitter.done()` into its completion. The tap's `done`
never ran, and a downstream `whenever` over the block never ran its `LAST`
(#18472).

The block now counts the `done`s its `whenever` bodies make while it opens
their sources and completes right away when there was one, exactly as for a
`done` in the block body. The completion also runs a downstream done that is
itself a chain of markers (a `whenever`'s `LAST` phasers plus its block's
done group), which it used to drop. Red's live ResultSeq supply builds its
`head` and `sort` stages this way (`t/190-resultseq-supply`, "sort flushes a
finite upstream window in pipeline order").
