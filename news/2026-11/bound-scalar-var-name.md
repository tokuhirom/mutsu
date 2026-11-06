# `.VAR.name` of a `:=`-bound scalar

`my $c := $x; $c.VAR.name` now answers the aliased variable's name `$x` (it answered `$c`),
following a chain of binds to the root, as rakudo's container descriptor does (#16931).
