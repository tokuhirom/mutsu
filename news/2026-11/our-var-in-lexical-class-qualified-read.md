# `our` variables of a lexical class are reachable as `$X::v`

`my class X { our $v = 3; our @a; our %h }` stored its package variables under the class's mangled storage name only, so `$X::v`, `@X::a` and `%X::h` read an empty placeholder. `set_our_var` now also stores the container under the user-facing spelling. The `our sub` of a lexical class (`X::f()`) is a separate gap.
