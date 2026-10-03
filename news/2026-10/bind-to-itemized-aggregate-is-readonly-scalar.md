# `:=` to `$(%h)` / `(1, 2).item` binds a readonly Scalar

`.item` / `$(...)` puts an aggregate in a `Scalar`, so `my $x := $(%h)` binds
`$x` to that Scalar. In rakudo, `$x.VAR.^name` is `Scalar`, and `$x = 3` fails
with "Cannot assign to a readonly variable or a value". mutsu treated the bind
as a bind straight to the aggregate: `.VAR` reported `Hash`/`Array`/`List`,
and an itemized List even produced the immutable-value error.

A `$` name bound to an itemized aggregate is now recorded as a readonly
binding that owns a container, matching rakudo on all three shapes (#11129).
Binding an unitemized aggregate (`my $z := {a => 1}`) still has no Scalar.
