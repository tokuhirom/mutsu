# `make` writes the one Match's `.made`

A grammar action's `make` used to build a copy of the Match carrying the
value, and the post-parse action walk rebuilt the whole parse tree out of
those copies. A Match that user code kept before its action ran — a code
block pushing `$<num>` onto an array, an action keeping its `$/`, a wrapped
token's `callsame` result — therefore never saw its `.made`, and was not
`===` to the node in the finished tree (#12954).

`.made` is now shared state of the capture node (ADR-11404 rule 6):
`make` and `Match.make` write it in place, and the action walk dispatches
each node's action without rebuilding the tree. Besides matching rakudo,
the walk no longer allocates a new attribute map and Match per interior node.
