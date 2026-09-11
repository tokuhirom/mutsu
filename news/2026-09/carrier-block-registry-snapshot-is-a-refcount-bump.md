# A carrier block's registry snapshot is a refcount bump, not a copy of every routine in the program

`eval_block_value_inner` — the carrier-block entry point that runs a `where`
clause, a regex code block, a role type argument, an `EVAL`'d unit — opened by
snapshotting the three registry tables a block-scoped declaration has to be
rolled back out of:

```rust
let mut saved_functions   = self.registry().functions.clone();
let saved_proto_subs      = self.registry().proto_subs_snapshot();
let saved_proto_functions = self.registry().proto_functions.clone();
```

All three were plain `HashMap`s, so that was an O(registered routines) deep copy
— taken **unconditionally**, on every carrier block, purely so the block *could*
be rolled back if it declared something.

The *restore* had already been made conditional (#7858's neighbourhood): it
snapshots `registry_write_gen`, and skips the whole rollback when the generation
is unchanged, because the overwhelmingly common block — a grammar `token` body
that is just a regex literal — writes nothing to the registry. The snapshot
never got the same treatment, so the copy was made and thrown away unused
several thousand times per parse.

## The fix is the mechanism the interpreter already had

`Interpreter`'s program-global symbol tables are `Arc<HashMap<..>>` copy-on-write
shares: reads go through `Deref` unchanged, and every write goes through
`Arc::make_mut` (`cow_table_mut`), which copies only while someone else still
holds the table. That is why the two operator tables snapshotted on the very next
lines of the same function — `operator_assoc`, `user_declared_infix_ops` — were
already free.

`Registry::functions`, `Registry::proto_functions` and `Registry::proto_subs`
now join that group. The snapshot becomes three refcount bumps; the copy moves
onto the block's first registry *write*, and only happens if the snapshot is
still alive then. Nothing about the semantics changes — a block that declares a
routine still gets its own copy and still does not leak it — only the *timing*
of the copy, exactly as the same refactor did for thread spawns.

Reads were untouched (that is what `Deref` buys), so the diff is the ~30 write
sites, which the type system finds for you: `Arc<HashMap>` has no `DerefMut`, so
every `registry.functions.insert(..)` is a compile error until it becomes
`registry.functions_mut().insert(..)`. There is no way to write the map that
bypasses the copy-on-write.

`snapshot_routine_registry` — taken whenever a routine that declares inner
`my sub`s is entered — snapshots the same three tables, so it got the same win
for free.

## Measured

The issue's own repro: `Config::TOML::Parser::Grammar.parse($toml, :$actions)`
over an 8-line TOML document, with `Crane` on `MUTSULIB`, release build.

| | before | after |
| --- | --- | --- |
| whole program (callgrind `Ir`) | 3,302,258,259 | 3,233,951,370 (−2.07%) |
| `RawTable::clone` self cost | 66,350,285 (2.01%) | 39,473,707 (1.22%) |
| `eval_block_value_inner` → `RawTable::clone` | 2,076 calls / 11,268,065 `Ir` | *absent* |
| wall clock, median of 7 | 0.468 s / 0.478 s | 0.438 s / 0.425 s |

The instruction counts are the trustworthy half (callgrind is deterministic);
the wall-clock pair is two independent A/B rounds on a 4-core container, and
moves further than the instruction count because the copy it removes was also
allocator traffic.

The saving scales with **how many routines are loaded**, not with what the block
does, which is why it shows up on a module-heavy grammar workload and barely at
all on a self-contained benchmark.

## Pins

- `t/vm/scope/carrier-block-registry-scope.t` — the behaviour the snapshot
  exists for, end to end: a `sub` declared in a bare block, a `do` block, a
  `where` clause and a regex code assertion is lexical to it, and the outer
  routine of the same name comes back. All ten assertions match Rakudo.
- `routine_tables_snapshot_by_refcount_and_copy_on_write` (`src/runtime/registry.rs`)
  — the mechanism itself: the snapshot shares the live table, a write copies,
  and the copy does not reach the snapshot.

Closes #7887.
