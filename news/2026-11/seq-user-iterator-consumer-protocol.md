# Seq consumers call a user iterator's methods the way rakudo does

On a `Seq.new($iterator)` over a user iterator, each consumer now calls the
same iterator method rakudo does:

- `.sum`, `.join`, `.min`, `.max`, `.minmax`, `.gist`, `.reduce`,
  `.classify`, and the `Set`/`Bag`/`Mix`/`Map`/`Hash` coercions walk the Seq
  with `pull-one`. Before, they reified it as a List, which called the
  iterator's own `push-all`.
- `.sink` calls the iterator's own `sink-all` instead of pulling one value at
  a time.

An iterator that honours the protocol gives the same values either way. Only
an iterator whose methods have side effects can tell the difference.

The `sink` statement prefix is now an explicit `.sink` call, as on rakudo.
`sink $s` runs a Seq held in a variable (a gather or a `.map`), calls a user
`sink` method, and still does not force a lazy array or list. An unhandled
Failure sunk with `$f.sink` now carries its fail-site frames, as `sink $f`
always did.
