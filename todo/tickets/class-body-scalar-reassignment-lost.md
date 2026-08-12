# Class-body scalar reassignment writes a key nothing reads

A plain reassignment of a class-body `my` scalar by a LATER body statement
never lands in the store methods close over:

```raku
class C {
    my $x = 10;
    $x = 20;
    method x { $x }
}
say C.x;   # raku: 20, mutsu: 10
```

## Root cause

Each class-body statement compiles as its own chunk under the class package
(`compile_decl_stmts_chunk_in_package`, `src/compiler/decl_plan.rs`), so
reads and writes of a sibling statement's `my` lexical are name-mediated
through `env` — and the name forms are asymmetric:

- The `my $x = 10` chunk flushes the local to env under the bare sigil-less
  name `"x"` (`flush_local_to_env`).
- The `$x = 20` chunk compiles via `emit_set_named_var`
  (`src/compiler/mod.rs` ~1788) which package-qualifies: `SetGlobal("C::x")`.
  The value lands under `"C::x"`, which nothing reads back.
- At body exit `persist_class_body_statics`
  (`src/runtime/registration_class_body_exit.rs`) copies the BARE env key
  (`"x"` = 10) into `package_lexicals[C]` — and its skip check
  (`env.contains_key("C::x")`) can even drop `x` from the store entirely.

The READ side of the same asymmetry (auto-qualified `%C::h` / `@C::a` reads
missing the bare-name env entry) was fixed 2026-08-13 by
`auto_qualified_bare_env_read` (`src/vm/vm_env_helpers.rs`, pinned by
`t/class-body-lexical-read.t`); scalar reads already had the equivalent
fallback in `GetGlobal`. The WRITE side remains: hash/array ELEMENT writes
use the bare name (`index_assign_target_name` never qualifies) and so land
correctly, but whole-scalar (and presumably whole-hash/whole-array)
reassignment goes to the qualified key.

## Fix directions (pick one)

1. Make `persist_class_body_statics` prefer the qualified env key
   (`C::x`) over the bare one when both exist for a declared static —
   smallest fix, makes the method-visible copy correct; in-body reads
   already see `C::x` via `GetGlobal`'s qualified-first lookup.
2. Make `emit_set_named_var` not qualify a name that an earlier chunk of
   the same class body declared (needs cross-chunk declared-name tracking
   in `class_body_plan`, which `class_declared_static_names` already has —
   thread it into the chunk compiler's local_map or a no-qualify set).

Found during the Text::CSV 79_callbacks campaign (the hash-read twin was
the actual blocker; this scalar residue is not needed by the CSV suite).
Repro: `tmp/repro-b2.raku` shape above.
