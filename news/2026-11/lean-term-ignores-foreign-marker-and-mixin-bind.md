# A lean sigilless term's declaration ignores a same-named marker and binds a mixin value

Two things kept Monad's type-cache helpers (`my \key = ...; my \hit = $cache{key}`, run ~30
times per `t/14` chain iteration) off the env-free declaration lane added in #17376:

- a routine's sigilless parameter of the same name (`sub publish(Str \key, ...)`) writes
  `__mutsu_sigilless_readonly::\key` / `__mutsu_sigilless_alias::\key`, which latch the name as
  an env key for the whole program; every later `my \key = ...` then left the lane to clear a
  marker no reader of a lean term consults. A lean term now ignores the readonly marker (its
  slot flags decide), and its store clears a visible alias key itself (`clear_lean_term_alias`),
  as the general store does;
- a term bound to a role-mixin value (`$P.^pun but Some`, Monad's cached types) was declined by
  the bound-term lane's inert-payload test; a mixin value is a value here and now binds.

Monad `t/14_concurrent_types.t` chain iteration (callgrind slope, profiling build): **6.52M ->
6.04M Ir** (-7.4%); general-store runs per iteration 77 -> 23. Full `t/14` ~84 s -> ~70 s
(profiling build) vs raku ~11 s. Combined with #17669, #17670 and #17682, the iteration is down
from 7.57M to 6.04M (-20%) this session (#17331).
