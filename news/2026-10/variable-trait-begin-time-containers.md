# `@`/`%` variable traits apply at BEGIN time too

A user `trait_mod:<is>(Variable ...)` handler on an `@` or `%` declaration in a
nested scope is lifted into the BEGIN prologue the same way scalar ones are
(#12403). `my @a is env` and `my %h is env` in blocks each preceded by a
`BEGIN` that sets `%*ENV` now see their own `BEGIN`'s state, and a hash-valued
`$v.var = ...` result is coerced to a Hash. Found with Trait::Env, whose
`t/09-basic-variable.rakutest` now passes.
