# A Proxy argument takes part in multi dispatch by its fetched value

A `Proxy` passed to a multi sub (through a `:=`-bound variable or a sigilless parameter) ranked
by the type `Proxy`, so `multi f(%h)` and `multi f(Hash $h)` lost to `multi f(Mu $x)`. The
ranking now runs over the arguments' FETCHed values, and a `@`/`%` parameter binds the fetched
container instead of failing with "expected Associative but got Proxy" (#12907).
