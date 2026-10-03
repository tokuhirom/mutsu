# Native types are subtypes of their boxed types, with honest REPRs and NativeHOW traits

`int32 ~~ Int` used to be False and `int32.^mro` was `int32, Any, Mu`. The core native types now
sit in the builtin-type catalog with raku's MROs:

- `int*`, `uint*`, `byte` and `atomicint` sit under `Int`.
- `num*` sits under `Num`.
- `str` sits under `Str`.

Their type objects report `.REPR` as `P6int` / `P6num` / `P6str`.

The `native` declarator now records the traits rakudo's `NativeHOW` takes:

- `is ctype<...>` stores MoarVM's C-type code in `.^nativesize` (`long` is -4, `size_t` is -6).
- `is nativesize(N)` stores `N` in `.^nativesize`.
- `is unsigned` sets `.^unsigned`. It used to be parsed as an unknown parent class.
- `is repr<...>` sets `.REPR`.

On an ordinary class these traits fail as they do in rakudo, because only `NativeHOW` has the
setters. They are never dispatched to a user `trait_mod:<is>`.

With this, upstream `NativeCall::Types` loads unmodified and gets as far as `CArray`'s
representation, which is #11209. This is a slice of ADR-11203 (#11204).
