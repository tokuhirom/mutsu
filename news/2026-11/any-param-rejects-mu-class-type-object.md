# An `Any` parameter rejects the type object of a `class M is Mu`

The type check answered `Any` for every type object by name. Only the
smartmatch path corrected that against the class's MRO, so `M ~~ Any` was
False while `sub f($x)`, `sub h(Any $x)`, `my Any $a = M` and `Any`-typed multi
candidates all accepted `M` (#18804).

The MRO check now lives in the shared type check (`class_type_object_outside_any`)
and the smartmatch copy is gone. That copy also misjudged built-in classes,
whose registered MRO is truncated before `Any, Mu`: `X::TypeCheck::Binding ~~ Any`
and `X::AdHoc ~~ Any` were False, and are now True as on raku.
