# `defined %h.AT-KEY('missing')` is False

The `defined` function answered `True` for an explicit `Hash.AT-KEY` call on a
missing key: the argument arrives as the lazy `HashEntryRef` element slot, and
the function's own match counted any non-type-object value as defined.
`defined EXPR` now reads the slot and then delegates to the one `.defined`
implementation (`native_method_0arg`), as `elems` already did, so the function
and the method can no longer drift apart. Found with Hash::LRU 0.0.8
(`ok ! defined $cache.get('a')`). Pinned by
`t/types/defined-function-reads-hash-at-key-slot.t`.
