# A keyed `CORE::` read answers one key instead of building the core stash

`CORE::{$rev}` and `CORE::{$key}` with a run-time key used to materialize the
whole `CORE::` stash -- about a thousand `&name` resolutions, each with a
`user_declares_routine` probe whose cost grows with the number of loaded
routines -- only to index one entry of it. Every key the stash can hold is now
decided by the key alone: a `&name` key resolves that one built-in routine,
and every other key (`v6c`, `v6d`, `v6e`, ...) is answered by the stash's lazy
`AT-KEY`, as it already was after the whole build.

On a release build with `use Test` loaded, `CORE::{$rev}.WHO{$type}` went from
about 3 ms to 0.09 ms per lookup. Rakudo-Type-Introspection's
`t/02-pseudostash.rakutest`, which reads `CORE::{$rev}.WHO{$name}` for every
revision of every type reachable from `MY::`, dropped from 30 s to 2.0 s
(rakudo: 0.7 s). Closes #17776; refs #17698.
