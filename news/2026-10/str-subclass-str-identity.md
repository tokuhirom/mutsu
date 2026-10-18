# A `Str` subclass's `.Str` is the instance, through the native base

The by-name gate that answered `.Str` of a `Str` subclass with the instance itself is gone
(ADR-11276 §9.183): the native-base resolution's list of coercers that return the instance (the
one `MyInt.new(5).Int` uses) now has `Str` and `Stringy` for a `Str` payload, so `.Stringy` gives
the instance too, as in Rakudo.
