# `.=` stores one method call; the rest of the chain applies to the result

`$x .= meth.more` is `($x .= meth).more` in rakudo: only the first method is
the mutator, and a further postfix runs on what the assignment returns. The
statement-level `.=` shortcut folded the whole chain into the stored value
instead (`$x .= uc.flip` stored `"BA"`; `@a .= map(...).sum` stored the sum),
and the expression-level one rewired the chain's outermost call to the
target, dropping the first method entirely: `$c .= new.add(2)` became
`$c = $c.add(2)`.

`CSS::Stylesheet`'s `$stylesheet .= new(:imports, :$media).parse($css)` kept
parsing into the previous stylesheet, so `t/import.t` saw every rule twice
and no media filtering; all nine of its test files now pass, matching raku.
