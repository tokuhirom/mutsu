# `.new` on a mixin type object builds the mixin class

`(B but R).new(...)` used to construct a bare `B` and then mix `R` into the new
object, which reblessed it into the class `B+{R}` and composed the role's
attributes onto it a second time, per instance. The named arguments never
reached the role's attributes, so `(B but R).new(:type<given>).type` answered
the role's default instead of `given` (#17539).

As in Rakudo, `B but R` is now the class `B+{R}`, composed once (role by role,
in the order the old path applied them), and `.new` builds an instance of that
class: its construction initializes the role's attributes from the named
arguments like any other attribute and runs the base's and the role's `TWEAK`
in order. A parameterised role and a base with its own `new` keep their
existing path.

Monad builds every value as `(pun but Some).new(:$value)`; that construction no
longer pays a role composition each time (#17331).
