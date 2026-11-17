# A sigil-less stash key does not find a `$` variable

A stash subscript key is the symbol as spelled. `our $x = 5; OUR::<x>` and
`package P { our $y = 1 }; P::<y>` are `(Any)` on rakudo, because the stash
holds `$x` / `$y`, not `x` / `y`. mutsu's `Stash` `AT-KEY` retried a sigil-less
key with a `$` prepended and answered the variable. It now looks the key up as
written; `OUR::<$x>` still finds the variable and `P::<&f>` the routine. Part
of #17846.
