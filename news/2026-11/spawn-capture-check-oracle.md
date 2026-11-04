# `MUTSU_SPAWN_CAPTURE_CHECK`: which aggregate lane writes the spawn capture analysis misses

ADR-12204 proposes capturing a spawned block's `@`/`%` lexicals as shared cells, which is
sound only for names the capture analysis sees. `MUTSU_SPAWN_CAPTURE_CHECK=1` records the
aggregate names each `start`/`Promise.start` block references and reports, once per
shape on stderr, a later write to an aggregate's name-keyed lane (`@a`, `%h`, or the
`__mutsu_atomic_arr::`/`__mutsu_atomic_hash::` element lanes) for a name no spawn
captured; `=all` also prints every spawn's captured set. Spawn seeding is not counted.
Behaviour is unchanged and the hooks are one cached-flag test when the variable is
unset. Run over the concurrency tests and the whitelisted `S17-*` roast files it names the
shapes the analysis must learn first (block-less supply/proc/scheduler spawns, instance
and dynamic aggregates, registered method bodies). Slice S1 of ADR-12204, #16404.
