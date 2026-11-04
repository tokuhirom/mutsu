# An omitted optional `num` / `str` parameter binds its zero

`sub f(num $v?) { $v }; f()` answered the type name `num`; it is `0e0` in
rakudo, as `int $v?` was already `0`. The same holds for `num32`/`num64` and
`str` (`""`), in subs, pointy blocks, methods and submethods. With that,
`roast/MISC/misc.t` passes its first five subtests like rakudo (which itself
aborts at the sixth); the ledger row now points at that sixth (#16412).
