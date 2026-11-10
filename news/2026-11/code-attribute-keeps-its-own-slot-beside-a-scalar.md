# `&!f` no longer shares a slot with a same-named `$!f`

A class (or a subclass) declaring both `has $!f` and `has &!f` stored them under
distinct keys already, but reading `&!f` (`(&!f)()`, `&!f(...)`, `-> { &!f }`)
and assigning it (`&!f = ...`) went through the scalar attribute's `!f` local,
so the call hit `Nil` or overwrote the scalar. `&!f` now reads and writes the
code attribute's own storage slot, like `@!f` and `%!f` always did
(Test::Describe's `Match::Change`).
