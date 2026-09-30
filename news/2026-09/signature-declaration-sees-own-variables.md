# Signature declarations see their own new variables on the RHS

`my ($x, $y) = $x, 2` inside a block used to raise `X::Redeclaration::Outer` (or
read the outer `$x` through a nested block). The parser now declares the plain
targets, default-initialized, before the destructure temp is evaluated, so the
RHS reads the new `Any` bindings exactly as rakudo does (#10173).
