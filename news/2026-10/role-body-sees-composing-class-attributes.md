# A role body sees the composing class's attributes

A role body runs once per composing class, and Rakudo runs it after the class's own
`has` declarations exist, so `my @attrs = ::?CLASS.^attributes` inside the role lists
them, with custom attribute traits (`has $!x is hidden-from-ValueType`) already applied.
mutsu composes header roles before the class body registers anything, so the list was
empty and `ValueType`'s `.WHICH` / `is rw` check never saw an attribute.

While a header role's deferred body runs, the class's own attribute declarations are now
published as stand-ins (without the role's own attributes, as in Rakudo) and their custom
attribute traits are applied to them; both are removed afterwards and the real class body
registers them as before. The body also sees the names of the role's declaring scope
(`my role Excluded {}` in the role's module), which previously resolved to a bare string
when a class in another file composed the role.

The `ValueType` distribution's `t/01-basic.rakutest` now passes. A custom attribute trait
runs once more than in Rakudo when a role with a body is composed (the stand-in pass).
