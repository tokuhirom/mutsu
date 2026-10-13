# A read-only `* + *` takes the light closure call

A WhateverCode with several stars (`* + *`), or with one star next to `$_`,
used to bind every placeholder parameter `is raw`. The light closure call
declines a raw parameter passed a variable, so `(* + *)($x, 1)` ran through the
full closure entry.

Each numbered placeholder is now `is raw` only when the body mutates it or
needs its container (`*++`, `++*`, `* =:= *`, `*.=foo`), the rule the
single-star `_` parameter already followed. The mutation scan is shared by both
paths (`src/whatever_curry/mutation.rs`). Per call, `$f($x, 1)` with
`$f = * + *` went from 38.1k to 5.3k Ir, and `f($x, 1)` by name from 41.1k to
11.3k.

Closes #13310.
