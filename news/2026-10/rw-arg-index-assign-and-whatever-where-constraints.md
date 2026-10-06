# `is rw` parameters bind an assigned element; `where * > N` constraints apply

Found with the Path::Map distribution. An indexed assignment passed as a call
argument (`f(%h<a><b> = $v)`, `$f(@a[1] = $v)`) now yields the element's own
container, so an `is rw` parameter writes through to it, as in Rakudo. A scalar
assignment argument to a code-variable call (`$f($x = 1)`) does the same.

Separately, `Parameter.constraints` for a `where * > 43` clause returned a thunk
that handed back the WhateverCode itself (always truthy); it now applies the
WhateverCode to the topic, so `10 ~~ $param.constraints` is `False`.

Path::Map's `t/trait.rakutest` and `t/validation.rakutest` now pass.
