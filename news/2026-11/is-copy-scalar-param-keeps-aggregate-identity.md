# `$a is copy` keeps the aggregate, `my @b := $mixed-array` binds

`sub f($a is copy)` copies the Scalar, not the Array or Hash it holds: `$a.push(3)`
reaches the caller's `@x` and `$a === @x` is true, as in rakudo (it used to copy
the aggregate). `@a is copy` and `%h is copy` still bring a fresh container.
Binding an itemized `but`-mixed Array (`my $a = [5, 6, 7] but M; my @b := $a`)
no longer fails the Positional check: the `Scalar` around the mixin is read
through, like the plain itemized Array.
