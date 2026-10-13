# A read-only `* + *` takes the light closure call

A WhateverCode with several stars (`* + *`), or with one star next to `$_`,
used to bind every placeholder parameter `is raw`. The light closure call
declines a raw parameter passed a variable, so `(* + *)($x, 1)` ran through the
full closure entry.

Each numbered placeholder is now `is raw` only when the body mutates it or
needs its container (`*++`, `++*`, `* =:= *`, `*.=foo`, an assignment to it),
the rule the single-star `_` parameter already followed. The mutation scan is
shared by both paths (`src/whatever_curry/mutation.rs`), and it now also counts
a direct assignment to the placeholder, so `(* += 1)($x)` writes back to `$x`.

Closes #13310.
