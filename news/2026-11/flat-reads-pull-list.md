# `flat` reads a `Channel.list` it descends into

A `Channel.list` (and a supply's lazy `.list`) is pulled on demand and caches
nothing until it is read. The pure flattener read that empty cache, so
`(Channel.list, 9).flat` answered `(9)` and `($channel.list,).flat` answered
`()`. `flat` already asks the interpreter for an `Iterable` instance's
elements before flattening; a pull list that has not been read to its end now
takes the same route and is read first, matching rakudo's `(3 1 2 9)`.
