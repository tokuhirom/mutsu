# A role's own `method new` sees the defaults of its type parameters

`role R[$f = 5] { has $.c = $f; method new { self.bless } }; R.new.c` answered
`(Any)` where Rakudo answers `5`. Calling a method on a bare role whose type
parameters all have defaults puns it to its default parameterisation, but the
VM call path excluded `new` from that (it assumed `dispatch_new` did it), so a
role that declares its own `new` ran it on the unpunned role and `$f` was
unbound inside it. The pun now applies to a role's own `new` as well.
Fixes #11652.
