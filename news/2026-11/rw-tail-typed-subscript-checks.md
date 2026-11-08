# An `is rw` tail subscript runs the key / bounds checks of a plain read

`sub f() is rw { %h{'foo'} }` on a typed-key hash (`my %h{Int}`) and
`sub g() is rw { @shaped[10] }` returned a deferred token without checking the
key or the bounds, so they did not die. The container-mode subscript now runs
the plain read first for those receivers and keeps its error, as raku does
(#17133).
