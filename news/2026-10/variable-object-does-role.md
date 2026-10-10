# A role mixed into a variable trait's Variable stays on the Variable

`$v does R` inside a `trait_mod:<is>(Variable:D $v, ...)` handler mixes the
role into the reflective `Variable` object. mutsu relayed that mixin to the
variable the object reflects, so the variable became a `Variable+{R}` object
and `$v.var` answered the mixin. The mixin now stays on the `Variable`, which
keeps answering `.var`, `.name`, `.block` and `.var = ...`. This is what
Injector's `is injected` on a variable needs: its `t/02-test.rakutest` passes
10/10.
