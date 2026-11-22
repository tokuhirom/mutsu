# `Cool.subst-mutate` and `Cool.substr-rw` on numbers and `IO::Path`

`subst-mutate` (6.c, deprecated in 6.d but still in rakudo) is declared on
`Cool` as well as `Str`. mutsu only had the `Str` row, so `$path.subst-mutate(…)`
on an `IO::Path` or `$n.subst-mutate(…)` on an `Int` died with "No such
method". The mutating method table now gives numbers and instances of built-in
`Cool` classes the `Cool` owner. `Cool.subst-mutate` substitutes in the
invocant's `.Str` and stores that `Str` back when something matched; a miss
leaves the invocant as it was, as in rakudo. `Cool.substr-rw` (`$n.substr-rw(0,1)
= "9"`) reaches numbers through the same owner.
