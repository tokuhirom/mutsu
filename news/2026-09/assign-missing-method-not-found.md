# Assigning to an undeclared method now raises X::Method::NotFound

`$obj.nope = 1` on a method a class never declares used to fall through the
rw-lvalue dispatch path to a generic `X::Multi::NoMatch`, "No matching
candidates for method: nope" — the diagnostic reserved for a real multi
whose candidate signatures just didn't match the call's arguments. Rakudo
raises `X::Method::NotFound` here instead, the same exception a plain
(non-assignment) call to the missing method already gets in mutsu.

The rw-lvalue path now distinguishes the two cases by checking whether any
overload of that method name exists anywhere in the class's MRO: none at
all means the name is simply undeclared (`X::Method::NotFound`), while an
overload that exists but doesn't match this call's arguments is still a
genuine multi dispatch failure (`X::Multi::NoMatch`).
