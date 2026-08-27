# Retired `__mutsu_atomic_value::N` keys are never removed from `shared_vars_dirty`

Found 2026-08-27 while root-causing ADR-0062 (the legacy atomic lane anchoring
to a stale thread's private `env`).

## The observation

`reset_atomic_var_key` (`src/runtime/runtime_shared_vars.rs`) retires a
generation of the legacy atomic lane by removing both the
`__mutsu_atomic_name::<name>` mapping and the `__mutsu_atomic_value::<N>` slot
from the root `shared_vars` store. It does **not** remove `<N>`'s key from
`shared_vars_dirty`, which is a process-global `Arc<RwLock<HashSet<String>>>`
(`src/runtime/mod.rs:2242`, `Arc::clone`d into every thread by
`src/runtime/runtime_thread.rs:783`).

A `rust-gdb` breakpoint on `sync_shared_vars_to_env`'s update push shows the
leftovers directly. On this three-statement program:

```raku
my $x = 1;
my $go = Channel.new;
my $pB = start { $go.receive; cas $x, -> $v { $v } };
cas $x, -> $v { $v };
$x = 4;
Promise.allof(start { $x = 5 }).result;
$go.send(1); $pB.result;
say $x;
```

`dirty_keys` at the reconcile is
`{"__mutsu_atomic_value::1", "x", "go", "__mutsu_atomic_value::2"}` —
`__mutsu_atomic_value::1` was removed from the store several statements
earlier and can never resolve again.

## Why it is only a leak and not a correctness bug

`sync_shared_vars_to_env` iterates the dirty set and looks each key up in the
store; a retired key resolves to nothing, so no update is pushed. The other
reader, `is_shared_var_dirty`, is consulted for bare container names
(`withdraw_transient_lane_containers`) and, since ADR-0062, for the bare name
in `published_atomic_seed` — never for a `__mutsu_atomic_value::` key. So the
stale entries are inert.

The cost is unbounded growth of a process-global `HashSet<String>`: one entry
per lane generation, and a lane generation is created every time a plain
assignment retires the previous one and an atomic op re-creates it. A loop
that alternates `$x = ...` with `cas $x, ...` therefore leaks one ~26-byte
`String` per iteration for the life of the process, and every
`sync_shared_vars_to_env` call pays a lookup for each of them.

## Why this is a `tickets/` slice and not `deep/`

The fix is local and needs no design: `reset_atomic_var_key` and
`reset_atomic_var_key_decl` already hold the `value_key` they are retiring, so
they can drop it from `shared_vars_dirty` in the same breath. The only care
needed is to remove *only* the `__mutsu_atomic_value::` key and **not** the
bare name — the bare name's dirty mark is load-bearing for
`published_atomic_seed` (ADR-0062 D2) and for
`withdraw_transient_lane_containers`, and clearing it would silently re-open
the ADR-0062 bug.

## Affected files

- `src/runtime/runtime_shared_vars.rs` (`reset_atomic_var_key`,
  `reset_atomic_var_key_decl`)

## Suggested pin

A test that alternates a plain assignment and a `cas` in a loop and asserts
the final value is still correct (guarding the "don't clear the bare name"
trap), plus a Rust `#[test]` in `runtime_shared_vars_tests.rs` asserting the
dirty set does not accumulate `__mutsu_atomic_value::` entries.
