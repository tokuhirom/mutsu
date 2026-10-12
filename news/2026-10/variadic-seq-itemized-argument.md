# A Seq held in a scalar is one element of a `*@` parameter

`my $s = (1, 2).map({ $_ }); f($s)` bound an empty `*@a` (the deferred Seq was
flattened before its callback had run), and a Range or finite gather held in a
`$` variable was flattened too, where Rakudo keeps the scalar's value as one
item. The slurpy now reifies the Seq and takes it whole when the argument came
from a `$` variable; the same value passed directly still flattens (#12970).
