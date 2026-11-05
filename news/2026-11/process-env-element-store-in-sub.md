# An element store into `%*ENV` from a sub outlives the sub

`sub b { %*ENV<K> = 'v' }; b(); say %*ENV<K>` printed `Nil`: `%*ENV` lives in the
per-interpreter base tier, so the element store landed in the sub frame's overlay copy of the
hash and died with the frame. A whole `%*ENV = ...` already survived because it is published to
the process stash (ADR-11318).

Element stores (`%*ENV<K> = v`, `%*ENV{$k} = v`) and `:delete` now take the same route: the
process hash is installed into the frame before the store and published afterwards, and the
`%*ENV<K>` / `:exists` readers and the `run`/`Proc::Async` environment builders read the
published hash. A missing `%*ENV<K>` now reads as `Any`, as in rakudo.

Found by AI::Gator (`t/02-gator.rakutest`). Test: `t/collections/process-env-element-store-in-sub.t`.
