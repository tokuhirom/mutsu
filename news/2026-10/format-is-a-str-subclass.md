# `Format` is a `Str` subclass

6.e's `Format` is `class Format is Str does Callable`, but mutsu modelled it as an instance with a
`format` attribute whose methods were answered by name: `Format.new("%d") ~~ Str` was `False`,
and `.chars` and `.uc` died with `X::Method::NotFound`. A `Format` now carries its string the way
a user `is Str` subclass does, so every `Str` and `Cool` method answers on the string. The methods
`Format` declares itself (`raku`, `arity`, `count`, `directives`, `Callable`, `CALL-ME`, `AST`) and
`Formatter`'s `CODE` and `AST` are method-table rows, and the by-name dispatcher is gone
(ADR-11276 §9.153).

A class declared under a built-in's name (`class Instant {}`) used to inherit the built-in's
ancestry. It now inherits from `Any` as any new class does, and an instance smartmatches only its
own type.
