# The owner rows Rakudo declares are registered (ADR-11276, slice 5)

139 of the 566 recognition rows that had no method row are registered now, all
of them methods Rakudo declares on the owner. `Instant` and `Duration` declare
`Real`'s transcendental and rounding methods (72 rows over the shared
`numify` handlers); `iterator` (12 owners), `deepmap`, `eager`, `first`, `map`,
`sort`, `slice`, `minpairs` and `maxpairs` of the owners that redeclare them
reuse `Any`'s handler; `new` is a row of 23 built-in classes (`Mu.new`'s
`dispatch_new`); the quant hashes' `of`, `Pair.freeze`, `Map.AT-KEY`/`Str`/
`clone`, `Block.of`/`returns`/`clone`, `Cool.chrs` and `Cool.substr-rw` complete
the declared set. The rows that a shape would shadow are found by owner only, so
no call changes its answer. `scripts/method-rows-report.py` now lists 427
unregistered rows, almost all of them inherited-only (an ancestor declares them).
