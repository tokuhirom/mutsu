# `.Seq` no longer forces an infinite lazy list

`(1...*).Seq` used to die with `X::Cannot::Lazy` because the `.Seq` coercion
forced any lazy list. A genuinely lazy source is now left alone: a lazy `Seq`
is returned as is, and a lazy `.List`/`.Array` view is re-tagged as a `Seq`
without iterating it. `.is-lazy` stays `True`, as in Rakudo. Closes #13254.
