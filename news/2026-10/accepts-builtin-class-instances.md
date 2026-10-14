# `ACCEPTS` of Exception and built-in-subclass instances

`X2.new.ACCEPTS($x)` for an `Exception` subclass and `DD.new(...).ACCEPTS($d)` for `class DD is Date`
used to die with `X::Method::NotFound`. The shapeless method-table lookup now falls back to `Any`
when an instance's MRO ends at a built-in class whose catalog chain reaches `Any` (an `Exception`
subclass), and a subclass of a built-in class with its own `ACCEPTS` row answers from that row
(#13333, ADR-11276 §9.108).
