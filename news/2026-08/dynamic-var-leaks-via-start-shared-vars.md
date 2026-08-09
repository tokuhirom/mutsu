# Scalar dynamic variables no longer leak process-wide via `start`

A scalar dynamic bound or assigned while a `start` block was spawned
stayed visible process-wide after the declaring frame returned, instead
of disappearing like Raku's thread-local dynamics should:

```
sub s1() { my $*A := 1; start { 0 } }
await s1();
say (try $*A).raku;   # mutsu (before): 1     raku: Nil
```

The leak happened specifically when a `start` was spawned *while the
dynamic was in the frame env* and the spawned block did **not** itself
reference that dynamic.

## Root cause

`start` spawns via `clone_for_thread_excluding` (`src/runtime/runtime_thread.rs`),
whose seeding loop copies every parent env entry into a lineage-shared,
bare-name-keyed `shared_vars` store so a spawned worker can see ordinary
lexicals. The skip list excluded a handful of internal names and, oddly,
only one specific dynamic (`$*CWD`/`*CWD`) — every other `*`-twigil
scalar (`$*A`, `$*CRO-ROUTER-ROUTE-HANDLER`, ...) was seeded like an
ordinary variable. Nothing ever removed the seeded entry, so any later
lookup anywhere in the lineage fell back to the store and found the dead
frame's dynamic. This surfaced as a real Cro bug: `Cro::HTTP::Router`
binds `my $*CRO-ROUTER-ROUTE-HANDLER := self` around each request handler
inside a `start`; a later, unrelated route block's
`with $*CRO-ROUTER-ROUTE-HANDLER` picked up a previous request's leaked
handler and mis-resolved its plugin configuration.

## Fix

Excluded scalar dynamic-variable keys (`is_dynamic_var_name`, minus the
`@`/`%` sigils) from the seeding loop, and added the same filter as
defense-in-depth to `sync_shared_vars_to_env`. A spawned worker's env is
already a clone of the parent's, so it still reads the dynamic that was
live at spawn time fine — only the process-wide name-lane sharing that
outlives the frame is removed.

**Aggregate dynamics (`@*x`, `%*x`) were deliberately left seeded.**
Unlike scalars, aggregates in this codebase have no cell-based closure
capture at all — their cross-thread mutation visibility depends entirely
on this same name lane's atomic CAS mechanism, the same as any other
captured aggregate. A first version of this fix excluded all dynamics
uniformly and broke `roast/S17-promise/then.t`'s `@*FOO` subtest (a
`.then`-chained callback mutating a dynamic array): a promise chain
legitimately needs that lane to propagate an aggregate dynamic's
mutations between chained callbacks running on different threads. Only
scalar dynamics get the leak-prevention exclusion.

## Verification

- All five probes from the diagnosis (`tmp/tapdiag-dynleak2.raku`) now
  match raku exactly (`Nil` in every leak-shaped case, the value read
  correctly when the block genuinely captures the dynamic).
- `t/http-router-plugin.rakutest` (vendored Cro::HTTP suite) no longer
  aborts with "Too many messages" after subtest 4; the file now completes
  `1..7` with one remaining, separately tracked failure (subtest 5, an
  unrelated plugin-config propagation bug — see the ticket's original
  investigation notes).
- New pin: `t/dynamic-var-start-leak.t` (passes under both `mutsu` and
  `raku`).
- All 98 whitelisted `S17-*` concurrency roast files and the 13
  `S17-promise/*.t` files (including `then.t`'s `@*FOO`/`&*FOO` subtests)
  pass with no regressions.
