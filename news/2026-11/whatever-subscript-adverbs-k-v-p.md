# `*<a>:k`, `:v` and `:p` build a WhateverCode

`(%h,).map(*<a>:k)` died with "Cannot map a List using '()'" because a value adverb on a `*`
subscript lowers to a `__mutsu_subscript_adverb` call whose first argument is the bare `*`, and the
WhateverCode priming walkers (`count_whatever`, `contains_hyperwhatever` and the replace visitor) did
not look into that call, so nothing was curried and the call ran on the `*` itself. They now treat the
call's first argument as the subscripted target, and the compiler primes the whole call, as
`*<a>:exists` does since #15445 (#15446).
