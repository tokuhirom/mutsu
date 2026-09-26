# A sigilless name bound to a `my $` declarator is writable

`my \foo = my $ = -3; foo = 5` died with "Cannot modify an immutable Int"
(issue #9563). The declarator evaluates to its new Scalar, so Rakudo binds
`foo` to that container. `my \foo = my $x = 3; foo = 7` writes `$x` for the
same reason. The parser sent a declarator on the right-hand side down the
readonly path, which is meant for plain values.

`build_sigilless_bind_stmt` now splits such a declaration in two: the inner
`my $x = ...`, then an ordinary bind of `foo` to `$x`, which already takes the
container path (`MarkBind` / `MarkSigillessBind`). Every anonymous `$` parses
to the same internal name, so each anonymous one first gets a fresh name. With
that, `my \a = my $ = 1; my \b = my $ = 2` stay two separate containers.
