# `.Str` and `.Stringy` are method-table rows of `Mu`

`.Str` and `.Stringy` of a receiver whose own type declares none are now rows of `Mu` in the built-in method table
(ADR-11276 §9.93), and the name-matching arm in `methods_0arg/dispatch_core_coerce.rs` that used to answer them is gone.
`Mu.Stringy` is `self.Str`, as in Rakudo, so every receiver's `Stringy` now agrees with its `Str`: `Stringy` of an
`IO::Special`, an `IO::Handle`, an `Exception`, a `NumStr`, a `Signature` and a `Hash` subclass used to render something else.

Three bugs went on the way. `Stringy` of a lazy list threw `X::Cannot::Lazy` where `Str` rendered `...`. A quoted call on a
`utf8` variable (`$u."Str"()`) threw `X::Buf::AsStr` where `$u.Str` decoded. And `Blob.new(1).Stringy` named `Str` in its
error message instead of `Stringy`.
