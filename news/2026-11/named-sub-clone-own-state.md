# A clone of a named sub gets its own `state`

`sub f { state $k = 0; $k++ }; f(); f(); (&f but R)()` answered 2: a named
routine's `state` lives in its registration's scope, and a closure clone of
it (`&f.clone`, and `&f but R`, which is `.clone.^mixin`) kept running there,
sharing the original's `state` variables and `FIRST` flag. Anonymous blocks
already got their own.

A closure clone of a named routine now records its own state scope in its
routine cell, which `sub_state_scope_id` consults before the registration, so
it starts from 0 as on rakudo; `nqp::clone` (the representation copy) keeps
sharing the routine's state (#18165).
