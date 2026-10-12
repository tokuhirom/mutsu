# `item`, `sink`, `race`, `hyper`, `succ` and `pred` are method-table rows for every receiver

The name-matching arms that answered `item`, `sink`, `race`, `hyper`, `succ`
and `pred` for every receiver are gone. `Mu.item` and `Mu.sink` are rows every
receiver reaches. `race` and `hyper` reach only the `Iterable` receivers, as in
Rakudo. `succ` and `pred` were already rows for every type that declares them.

A few answers moved to Rakudo's:

- `$match.item` is the `Match` itself, not its matched string;
- `Bool.succ` and `Bool.pred` on the type object are `True` and `False`;
- `Int.succ` and `Str.pred` on a type object, and `Range.race` on the `Range`
  type object, raise instead of answering the type object.

Sinking a `gather`, a `map` or a `Failure` behaves as before.
