# A Callable held in a `&` variable no longer pulls a deferred Seq argument

`my &r = -> \s { 1 }; r((1..3).map(*+1))` pulled the `.map` Seq before binding
it, which made the Seq re-readable afterwards. Rakudo (and a named sub in mutsu)
keep the Seq single-use: a second `.List` throws `X::Seq::Consumed`. The
compiled-closure call path no longer reifies its arguments up front; the binder
already pulls a deferred Seq where a `*@`/`+@`/`@` parameter reads its elements.
Closes #13105.
