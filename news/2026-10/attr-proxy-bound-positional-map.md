# `@!attr.map` on a Proxy-bound user Positional calls its own `map`

Red's `relationship` trait `nqp::bindattr`s an `@` attribute to a Proxy whose FETCH returns a
`ResultSeq`. `@!aa-bb.map(*.bb-id)` took the mutable-array `map` route with the unfetched Proxy as
a one-element list, so the callback received the `ResultSeq` itself
(`No such method 'bb-id' for invocant of type 'AABB::ResultSeq'`). That route now FETCHes the
Proxy first and leaves a receiver whose class declares its own `map` to ordinary dispatch.
Red `t/54-hand-made-many-to-many` now passes.
