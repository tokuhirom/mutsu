# `=:=` tells an assigned scalar from a bare value

`my $x = 1; $x =:= 1` answered `True`, because a variable compared with a
literal fell through to a value comparison (#9769). `=:=` compares containers,
and an assigned `$x` owns a Scalar that no literal can be. The same was true
of a type-object term, so `my $x = IterationEnd; $x =:= IterationEnd` was
wrongly `True`.

A literal or a type-object/term bareword is now compiled like the existing
`.self` case, `ContainerEqDeconted`. The pair is identical only when the named
side was `:=`-bound straight to that value, as in `my $x := 1` or the
`(my $v := $it.pull-one) =:= IterationEnd` loop idiom. A bareword that names an
in-scope sigilless variable or constant still reads that lexical, so
`$x =:= t` for `my \t := $x` stays `True`.
