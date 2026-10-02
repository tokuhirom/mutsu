# `.share` runs a supply block when it is called, as in raku

`supply { ... }.share` used to defer running the block until its first
consumer tapped the shared Supply, so values the block emitted synchronously
went to that first tap and the block's side effects happened late. raku's
`Supply.share` taps its source on the spot: the block now runs once, at
`.share` time, with no consumer attached -- what it emits before anyone taps
is lost, and every consumer (a direct tap, a derived `grep`/`map`/`head`, a
`whenever`, a react) joins the already-running block. A block that dies while
starting quits the shared supply instead of throwing out of `.share`.

The lazy start-up hooks that `whenever`, react subscriptions and `.Promise`
needed to kick an untouched shared block (#10740) are gone with it. (#10839)
