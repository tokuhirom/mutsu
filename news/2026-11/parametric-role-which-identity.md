# A parameterized role type object has a stable `.WHICH` and is `===` itself

`R[Str, Int].WHICH` answered `Package|<counter>` (a fresh number per evaluation)
and `R[Str, Int] === R[Str, Int]` was False. It is now `R[Str,Int]|U<id>`, one
identity per distinct argument list, as in rakudo, and `===` compares base and
arguments. Found through Monad 0.3.0, whose `Monad::Either` keeps a type cache
keyed on `E.WHICH ~ '|' ~ L.WHICH ~ '|' ~ R.WHICH`. Monad's `t/13_subtype_hooks`
still stops at its last assertion (filed separately).
