# A `use` inside a routine replays the registry functions its import wrote

`sub f { use Test; 1 }` called in a loop re-imported the whole of `Test` on
every call: 2.08M Ir per call, 4000 calls in 1.33 s against 0.98 s for rakudo
including its start-up (#9170, `UseModule`'s re-import).

The replay of a block-scoped `use` (#13906) skips the import on later runs when
the first run only filled the block's lexical import set. `Test` exports a
`multi sub trait_mod:<is>`, a candidate that the routine registry keeps (it is
not bound lexically), so the first run also grew the function table and the
replay's "nothing else changed" probe failed on every run. The probe now
accepts exactly the registry functions the scope logged as import writes
(`ImportWrite::Function`); the replay re-inserts them, logs them for the
scope's exit and flushes the name-keyed resolution caches. Any other registry
write (a proto, a `proto_subs` marker) still disables the replay.

Callgrind slope (release build, warm, 100 -> 600 calls): 2.08M -> 47K Ir per
call; wall clock for the 4000-call loop 1.33 s -> 0.05-0.1 s. Pinned by
`t/tooling/use-in-block-replays-registry-function-writes.t` (output identical
under `raku`).
