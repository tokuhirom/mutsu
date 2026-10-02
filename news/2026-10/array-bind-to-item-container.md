# `my @b := @a[0]` binds the Array, not the element's Scalar

Binding an `@` variable to a single array element (`my @b := @a[0]`,
`@a[*-1]`) or to a `$` variable (`my @b := $s`) used to alias the Scalar
container itself. A later `@a[0] = [9]` or `$s = [5]` then changed what `@b`
held, and `@a[0] =:= @b` was True. As in rakudo, the `@` name is now bound to
the Array inside the container. Pushing through either name still reaches the
same Array, but replacing the container's content leaves `@b` alone. A slice
bind (`my @s := @a[1, 2]`) and an index known only at run time keep aliasing
the elements.

`=:=` now treats an `@`/`%` variable as the Array/Hash itself, never a
Scalar. Against an element, it is identical only through a shared bound
cell (`@d[0] := @c`). Against a `$` variable, it is identical only when that
`$` is `:=`-bound to it: `my $w = @z; $w =:= @z` is now False (#10692).
