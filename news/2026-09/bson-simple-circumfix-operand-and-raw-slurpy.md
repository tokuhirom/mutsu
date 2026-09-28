# BSON::Simple reaches ecosystem parity

BSON::Simple 0.0.2's `t/01-basic.rakutest` went from 37/92 to 92/92, so both of the
distribution's baseline files now pass (`t/02-special.rakutest` needs a compiled native
library and fails under rakudo too). The distribution exposed four interpreter gaps:

- **A user `circumfix:<...>` gets its whole semilist as one positional argument.**
  `⦃ hello => 'world' ⦄` used to pass the pair as a *named* argument, and `⦃ 1, 2 ⦄`
  used to pass two arguments. Like rakudo, the operator now receives one positional:
  the Pair itself, the List `(1, 2)`, or `()` for an empty circumfix. A pair written
  inside a user `postcircumfix:<...>` is positional too.
- **`**@x is raw` binds an un-itemized List**, not an Array, so a later `*@` slurpy
  flattens its sub-lists the way Hash::Agnostic's `new(**@values is raw)` relies on.
- **`Buf.write-int*` and `write-uint*` write the payload of an `Int` subclass
  instance** (`class Int64 is Int`) instead of zero.
- **An element explicitly assigned `Any` is data, not a hole.** A `Mu`-typed array or
  an `is default(...)` array now reads it back as `Any`, not the container default, and
  list assignment turns only `Nil` into the default. The declaration-initializer form
  (`my @d is default(42) = [Nil]`) is still tracked by #9902.
