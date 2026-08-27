# A `cas` on a `my` that shadows an outer name inside a `start` block leaks the worker's value to the outer lexical

Found 2026-08-27 while verifying ADR-0062 (the atomic lane anchoring to the
published value). **Pre-existing and independent of that change** — verified by
building the two touched files at their pre-ADR-0062 revision and observing
byte-identical wrong output.

## Minimal repro

```raku
# A: shadowing `my` in a start block, NO cas at all -- CORRECT
my $a = 1;
my $pa = start { my $a = 100; $a = $a + 1; $a };
say "A: ", $pa.result, " ", $a;      # raku: 101 1   mutsu: 101 1

# B: the same shape, but the worker uses `cas` -- WRONG
my $b = 1;
my $pb = start { my $b = 100; cas $b, -> $v { $v + 1 }; $b };
say "B: ", $pb.result, " ", $b;      # raku: 101 1   mutsu: 101 101
```

The worker's `my $b` is a fresh binding that merely shares a spelling with the
outer `$b`. Raku leaves the outer `$b` at `1`. mutsu leaves it at `101`: the
worker's value escapes its own block and overwrites the mainline lexical.

Case A proves the plain-assignment path already handles the shadow correctly
(`thread_redeclared_vars` does its job there), so this is specific to the
atomic path.

## Likely root cause

The legacy name-keyed atomic lane is keyed by the variable's **bare name** in
the **root** `shared_vars` store: `__mutsu_atomic_name::<name>` is a
`__mutsu_`-prefixed internal key, and `SharedStore::is_internal_key` /
`scope_for` deliberately resolve every internal key at the root lineage rather
than lineage-scoping it (`src/runtime/shared_store.rs`). That is correct for a
genuinely process-wide `atomicint` counter — all spawned threads must hit one
counter — but it means a worker's *shadowing* `my $b` and the mainline `$b`
share one lane entry, because nothing about the lane key distinguishes the two
bindings.

`builtin_cas_var` then marks the bare name dirty
(`dirty.insert(name.clone())`, `src/runtime/builtins_atomic_cas.rs`), and
`sync_shared_vars_to_env`'s blanket reconcile resolves that dirty bare name
through the lane and writes the worker's value into the awaiting thread's
`env`.

`thread_redeclared_vars` is the mechanism that is supposed to stop exactly
this, and `sync_shared_vars_to_env` does filter its dirty-key list through it —
so the open question for whoever picks this up is **why the worker's `my $b`
does not leave `b` in `thread_redeclared_vars` by the time the reconcile runs**
(a scope/lifetime mismatch between the mask and the `await`, most likely: the
mask is unmasked when the block's frame returns, but the reconcile happens on
the *awaiting* thread afterwards, where no mask was ever installed).

The code already acknowledges the underlying collision in prose — see
`atomic_cell_update`'s doc comment in `src/runtime/builtins_atomic.rs` ("the
legacy lane is keyed by the variable's bare NAME in a process-global store, so
an unrelated `my $i` anywhere else in the program reset the counter") and the
existence of `reset_atomic_var_key_decl`. This ticket is a concrete, reproducible
instance of it.

## Affected files

- `src/runtime/builtins_atomic_cas.rs` (`builtin_cas_var` — the bare-name dirty
  mark)
- `src/runtime/runtime_shared_vars.rs` (`sync_shared_vars_to_env` — the blanket
  reconcile and its `thread_redeclared_vars` filter)
- `src/runtime/shared_store.rs` (`is_internal_key` / `scope_for` — why the lane
  is root-scoped)

## Scope note

The durable cure is to retire the legacy name-keyed lane in favour of the
`ContainerRef` cell lane for every atomic scalar — a cell cannot collide,
because it carries binding *identity* rather than a name. That is an
ADR-0025/ADR-0013-scale campaign, not this ticket. A narrower fix that keeps
the worker's shadowed binding out of the lane (or out of the reconcile) is
worth attempting first; it should be pinned by the repro above plus the
`atomicint` counter shape (`my atomicint $n; await (^4).map: { start { $n⚛++ } }`)
to make sure the genuinely-process-wide case is not broken in the process.

## Related

- ADR-0062 §"Not addressed" (1) records the same bare-name collision as a
  known residual.
- `t/atomic-lane-stale-thread-anchor.t` pins the ADR-0062 behaviour and must
  stay green.
