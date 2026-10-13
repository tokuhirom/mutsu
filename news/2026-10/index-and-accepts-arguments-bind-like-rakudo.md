# A positional index and an ACCEPTS topic bind like Rakudo's candidates

`AT-POS` and `EXISTS-POS` on a `Range`, a `Uni`, a `Capture` or a `List` now bind an index that is
not an `Int` the way Rakudo's candidates do: `AT-POS` re-dispatches on the argument's `.Int` (a user
object's own method included, so `(1..5).AT-POS($obj)` works for any object with an `Int`),
`EXISTS-POS` answers `pos.Int == 0` as `Any`'s candidate does, `Capture` coerces every index through
`Int(Any)`, and a type object is `Cannot use 'T' as an index`.

`ACCEPTS` of the quant hashes, `Pair` and `Range` takes any topic: `set(1, 2).ACCEPTS(1..2)` coerces
the topic to a `Set` and is `True`, `(abs => 5).ACCEPTS(-5)` calls the method the key names and
compares truth, as documented, and `Range` compares a user object by `cmp`.

These are rows of the built-in method table (ADR-11276 §9.105); five name-matching arms of the
one-argument cascade and one in the runtime slow path are gone.
