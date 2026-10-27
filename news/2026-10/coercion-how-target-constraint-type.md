# CoercionHOW: ^target_type, ^constraint_type and ^nominal_target

`Int(Str).^target_type` is `Int`, `.^constraint_type` is `Str` (`Any` for `Int()`) and
`.^nominal_target` is the nominal target, as on Rakudo's `Metamodel::CoercionHOW`. They were missing,
so `Hash2Class` died on `No such method 'target_type'` while mixing in a class with a coercing
attribute.
