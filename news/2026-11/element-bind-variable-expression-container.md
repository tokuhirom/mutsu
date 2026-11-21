# An element bind of a variable answers the variable's container

`(%h<p> := $y)` is `$y`'s container in Rakudo -- the one the element now shares -- so
`my $r := (%h<p> := $y)` aliases `$y` and `(%h<k> := $x).VAR.^name` is `Scalar`. mutsu answered
the plain value there, so binding it gave a readonly copy (`$r = 9` died with "Cannot assign to
an immutable value") and `.VAR` reported the value's type (#18540). An element-bind expression
whose source is a scalar variable now answers the shared cell the store rebinds that variable to;
a bind of a plain value still answers the value itself (#18260).
