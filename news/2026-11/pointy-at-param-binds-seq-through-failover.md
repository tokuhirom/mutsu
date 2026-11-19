# An `@` pointy parameter of `with`/`if`/`given` binds a Seq like a signature does

A control statement's pointy parameter is a signature parameter: a Seq bound
to `-> @l` goes through `PositionalBindFailover` and the block sees its cached
List. mutsu lowered it to a declaration instead. `with EXPR -> @l` and
`if EXPR -> @l` became `my @l = $tmp`, so a `$`-held Seq — which since #18193
stays one element in an `@` assignment — gave `@l.elems == 1`; `given X -> @l`
and `with $var -> @l` became `my @l := topic`, whose `:=` check rejected a Seq
outright ("expected Positional but got Seq").

The `with`/`if` declaration now reads its value through `DeitemizeForBind`,
the `@`-parameter bind `for -> @a` already uses, and the `given` op binds a
Seq topic to an `@` pointy parameter through the Seq's cached List view
without writing anything back to the topic variable (#18257). This was the
Backtrace::Files `t/03-add-source-lines.rakutest` regression of nightly sweep
#109: `with path-lines($file) -> @lines` saw the whole file as one line.
