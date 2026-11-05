# `callsame` through a multi family visits `Any` before `Mu`

A multi family with `Mu`, `Any` and `Int` candidates deferred `Int` -> `Mu` and skipped `Any` when
`Mu` was declared before `Any` (the order that decided the chain was the declaration order, because
neither `Any` nor `Mu` counted as narrowing). `Any` is narrower than `Mu`, so the chain is now
`Int` -> `Any` -> `Mu` whatever the declaration order, for methods (the deferral expansion scores `Any`
as 2, `Mu` as 1 and an untyped parameter as `Any`) and for subs (the candidate sort puts fewer
`Mu`-typed positionals first). Picking between a lone `Any` and a lone `Mu` candidate also follows
rakudo now (#15721).
