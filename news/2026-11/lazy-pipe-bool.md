# The truth of a lazy `.map`/`.grep` pipe pulls its first result

`Seq.new($iterator).grep({ False }).Bool`, `(1, 2, 3).lazy.grep({ False }).so`
and `!(...)` of such a pipe answered `True` whatever the stage produced:
`Value::truthy` cannot pull, so a lazy pipe (a `LazyList` with a
`lazy_pipe`) read as true from its source, and the `.Bool`/`.so`/`.not`
method lane had no pull for it. The method lanes (`CallMethod` and
`CallMethodMut`) and the boolean chokepoint (`eval_truthy_impl`, `?`, `!`, a
condition) now pull exactly one result through the stage, as rakudo's
`Seq.Bool` asks its iterator for one; the rest stays lazy. Iter::Able t/31
and t/32 (`noone`/`every` over an `Iterator`) now match rakudo.
