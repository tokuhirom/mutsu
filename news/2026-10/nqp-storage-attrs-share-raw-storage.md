# `$!reified` and `$!storage` share the collection's storage

`nqp::getattr(@a, List, '$!reified')`, `nqp::getattr(%h, Map, '$!storage')` and a
Capture's `@!list` / `%!hash` now answer the raw storage Rakudo keeps behind a
high-level collection (ADR-12666 §3.6, #12669): a `BOOTArray` for a `List`, a
`BOOTHash` for a `Map`/`Hash`, and an `IterationBuffer` for an `Array`. `nqp::islist`
and `nqp::ishash` say 1 for them, two reads are the same object, and a `bindpos` /
`push` / `bindkey` through one is seen by the collection.

The other direction is shared too: `nqp::p6bindattrinvres(nqp::create(List), List,
'$!reified', $raw)` used to copy `$raw`'s elements into the new `List`, so a later
`nqp::push($raw, ...)` was lost. It now adopts `$raw`'s storage in O(1), as Rakudo
does, and the `List` sees every later write.
