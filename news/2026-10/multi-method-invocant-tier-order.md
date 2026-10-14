# A subclass's multi method candidate is ranked on its invocant, and unbindable candidates take part in the tiers

Rakudo types a multi method candidate's invocant by the class that declares it and compares it as the
first parameter, and it builds its topological tiers from the candidates' *declared* types, including
candidates that cannot bind the call. mutsu ranked method candidates on their non-invocant parameters
only, and dropped a candidate whose argument was unrelated to its parameter type from the tiers.

So a subclass's `multi method m(Associative $_ where {...}, |c)` lost to the parent role's
`m(Hash $d, |c)` even when rakudo ran it, because a third, non-binding `m(TH $hash)` candidate placed it
in an earlier tier. `MultiRank` now carries the declaring class (`invocant`) and `RankProfile` the
declared parameter types; a pair of method candidates in related classes orders by the invocant unless
the other candidate has a narrower declared parameter, and a candidate that cannot bind compares by
declared types alone (a tie or unrelated types leave the non-slurpy one narrower).

Fixes #13507 (PDF 0.6.17 `PDF::COS::Stream.COERCE`). Pinned by
`t/oo/method/multi-method-invocant-tier-order.t`.
