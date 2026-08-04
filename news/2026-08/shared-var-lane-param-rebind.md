# A re-bound `@`/`%` parameter no longer freezes on the shared-var name lane

The name-keyed `shared_vars` store is seeded once per name (`seed_if_absent`),
so it cannot represent two concurrently-live bindings of one name. A `@`/`%`
name re-bound per invocation through the **env-level parameter binding path**
and captured by a spawned block therefore froze at the first spawn's value:

```raku
say reduce -> $h, @words { $h + await start { [+] @words } }, 0, (1,2), (3,4);
# was 6 (3 + 3 — the second iteration's start saw the FIRST @words); now 10

say (await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100,101)), (2, (200,201))).join('|');
say (await map -> @K       { start { @K[0] } },        (300, 301), (400, 401)).join('|');
# line 2 was 100|100 — it read line 1's FIRST @K; now 300|400
```

Two holes, both in the exclusion machinery
`news/2026-08/start-block-destructured-array-param.md` introduced for
destructured sub-signature parameters:

1. **The recording was gated on `shared_vars_active`**, which is still false
   when the *first* spawn in a process runs. That spawn therefore consulted an
   empty set, seeded the destructured `@K` onto the lane anyway, and the frozen
   entry later poisoned any plain binding of the same name — the await-time
   `sync_shared_vars_to_env` pulled the stale array back over the parent's env,
   and every later closure captured it from there. `bind_sub_param_name` now
   records unconditionally.
2. **Only destructuring sub-signatures were recorded.** A runtime-invoked
   callback's plain `@`/`%` parameter (`reduce`'s `-> $h, @words`) is bound by
   the same env-level path (`bind_param_value`) with no local slot behind it,
   and froze the same way. `bind_param_value` now records those names too.

The recording also got sharper: `param_bound_aggregates` (renamed from
`sub_signature_bound_aggregates`) maps each name to the **container the binding
stored in env**, and `block_captured_scalars` only excludes a free variable
from the lane when the recorded container is identity-equal
(`same_container_arc`) to the one env currently holds. A stale entry from some
routine's parameter can therefore never mask an unrelated same-named outer
lexical — the ticket's original narrowing conditions, made per-binding instead
of per-name.

This was the remaining freeze behind `Digest::RIPEMD`'s wrong multi-block
digests (`todo/tickets/digest-dist-blockers.md` blocker 2): the compression
loop's `start` blocks read the reduce callback's `@words` frozen at its first
binding. With it fixed, the RFC-vector words are correct; what remains in that
dist is an independent anonymous-state bug
(`todo/tickets/anon-state-not-reset-per-block-clone.md`).

The general per-binding home for `@`/`%` captures that
`docs/recursive-start-shared-vars.md` defers is still future work; this fix
covers the env-level parameter binding path, which is where every observed
freeze lived.

Pinned by `t/shared-var-lane-param-rebind.t`.
