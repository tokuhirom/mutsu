# A `$t` declared next to a sigilless `\t` no longer hides it

`sub k(\t) { sub { my $t = 5; $t + t }() }` now yields `15`, as in Rakudo. A sigilless
binding and a same-spelled scalar share the storage key `t`, so `my $t` used to overwrite
the sigilless value. The compiler now parks the sigilless value in a slot under the term
key (`\t`) when it sees the scalar declaration and reads bare `t` from there for the rest
of the scope, including nested closures and methods.

This is a compile-time slice of #9962's "decide once" namespace move: sigilless parameters
and `my \x` still live under the scalar key, only a shadowed one is relocated.
