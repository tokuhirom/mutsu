# `.^meta` on a builtin metaclass skips the general method-call chain

`Type.^find_method(...)`, `.^find_private_method(...)` and the other
metamethods of the builtin metaclasses were `Type.HOW.meta(Type, ...)` through
the general method-call chain on the HOW instance: the native probe that
misses, then a dozen receiver gates, before the classhow dispatcher answered.
When the HOW is one of the builtin metaclasses and no wrap chain, `find_method`
intercept or user method can stand in front of the metamethod,
`try_builtin_how_meta_lane` now calls the classhow dispatcher directly.

Callgrind (profiling build, slope between 200 and 1200 calls, Ir per call):
`$a.^find_method('b-foo', :no_fallback(1))` 46.9k -> 26.9k,
`A.^find_private_method('zz')` 45.1k -> 26.5k (-43%). The remaining cost is the
call-site decode in front of the lane; the metamethod itself is ~4.5k.

A user `method ^name`, a custom metaclass and a wrapped method keep the general
path (`t/oo/mop/mop-meta-call-builtin-how-lane.t`).
