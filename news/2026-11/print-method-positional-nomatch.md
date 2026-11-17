# `.print`/`.note` with a positional fail their dispatch, not the lookup

`1.print(2)`, `1.note(2)`, `"a".print("b", "c")` and `Foo.new.print(1)` died
`No such method 'print' for invocant of type 'Int'`. `Mu`'s `print`/`note`/`say`/`put` are multis
every object has, so rakudo raises `X::Multi::NoMatch` listing `(Mu $:: *%_)`; mutsu now does too, for
any `Mu` multi a call reaches the not-found path with a positional for. A `Failure` held in a variable
(`my $f = open($bad); $f.print(1)`) exploded instead of raising the NoMatch, because the
`CallMethodMut` path lacked the check `CallMethod` had; both now share
`failure_print_no_match`, and the message names the invocant `Failure:U` as rakudo does. Closes
#17857.
