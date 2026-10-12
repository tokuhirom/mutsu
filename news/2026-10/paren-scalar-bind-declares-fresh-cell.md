# `(my $y := EXPR)` declares a fresh cell, like `my $y := EXPR`

An expression-position scalar bind was stored under its bare name with no local slot, so
`$q := $y` made `$q` a by-name reference and a repeated declaration (a loop body) re-bound
`$y` underneath it. It now takes a declaration slot as the statement form does.
`Hash::int`'s `push` (`$previous := $pulled` with `(my $pulled := $iterator.pull-one)` in the
`until` condition) paired every value with itself.
