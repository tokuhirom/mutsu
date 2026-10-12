# `of` and `keyof` are method-table rows, including rows owned by roles

`.of` and `.keyof` are now rows of the classes and roles Rakudo declares them
on: `Hash`, `Array`, `Blob`/`Buf`, and the roles `Positional`, `Associative`,
`Callable`, `Baggy`, `Setty`, `Mixy` and `QuantHash` (ADR-11276 §9.86). This
is the first time the method table has rows owned by a role. A role's rows
reach every receiver shape whose type composes that role. They come after the
composing class's own rows and before its parent's, which is how Rakudo
flattens role methods. A type object with no dispatch shape (`Positional`,
`Sub`, `Bag[Int]`, `Hash[Int,Str]`, `buf16`) finds the rows by walking its
base type's catalog MRO. The handlers read the type's parameters from its name.

This removes the last two name-matching arms of
`methods_0arg/dispatch_core_range.rs`, and with them the whole file, along
with five `of`/`keyof` blocks in the interpreter's slow path. Several answers
now match Rakudo where they did not before: `Map.of`, `Range.of`,
`Pair.of`/`keyof`, `Associative.keyof`, `Callable[Int].of`, `Regex.of`, the
quant-hash roles' type objects, `Stash.keyof`, `Bag[Int].keyof` and
`SetHash[Int].new(1).keyof` (each was `Mu` or `X::Method::NotFound`), a
`Hash[Int,Str]` bound to a `%` parameter (`.of` was `Int,Str`), and
`Map.new.of` (it was an empty type name). A `my %b is BagHash[Int]` value
keeps its declared type, so `.keyof` is still `Int`.
