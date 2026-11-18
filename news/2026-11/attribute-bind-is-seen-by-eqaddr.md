# A `:=`-bound attribute is that value to `=:=` and `nqp::eqaddr`

Since `nqp::eqaddr` became container identity (#17620), a `$!attr` operand was
always treated as a Scalar, so `$!next := $iter.pull-one` in one method and
`nqp::eqaddr($!next, IterationEnd)` in the next never matched. The nqp-style
iterator idiom then spun forever: the snip and span distributions' suites
hung, and Iter::Able's `group-conseq` called its transformer on
`IterationEnd` ("Unknown function: abs").

The bind already leaves a per-instance marker (it is what `nqp::isrwcont` and
`.VAR` read); container identity now consults it too, so a bound attribute is
its value and an assigned one stays its Scalar, as in rakudo.
