# A gather passed to a built-in method is read the way Rakudo reads it

`Set.new(gather { take 1; take 2 })` built an empty Set, and
`Supply.from-list(gather {...})` emitted the whole gather as a single value.
Some method calls handed a plain gather argument to the native method as the
gather's inner coroutine list, and the rest handed over the unpulled Seq. The
pure method code read either one as empty or opaque.

Both method-call paths now read a gather argument the way Rakudo's method does
(ADR-11678 §8.3). Constructors (`Set.new`, `Bag.new`, `Array.new`) and
`append`/`prepend` iterate it and consume it. `push`/`unshift` store the Seq as
one element. `Supply.from-list` pulls the gather, and a gather that dies
quits the supply, so a `whenever`'s `QUIT` phaser sees the exception. Every
other built-in method (`split`, `splice`, `polymod`) reads it through its cache
and leaves it readable. A user class's method still receives the Seq itself.
