# A package/module block no longer resets an outer lexical it never touched

```raku
my $x = "top";
module M { };
say $x;          # used to print (Any), now prints "top"
```

`package P { }` failed identically; `class C { }` and a bare block `{ }` never
had this bug (they exit through different scope-cleanup paths).

## Root cause

`exec_package_scope_op` (`src/vm/vm_misc_reduction_scan.rs`) restores the
mainline `locals` slots a `package`/`module` body might have written through
to `env` when it exits. The restore loop pulled a value out of `env` for
*every* declared local in the whole compiled unit, unconditionally:

```rust
self.locals = saved_locals;
for (idx, local_name) in code.locals.iter().enumerate() {
    if let Some(val) = restored_env.get(local_name).cloned() {
        self.locals[idx] = val;
    }
}
```

mutsu's `locals`/`env` dual store is lazy and per-slot: `env`'s bare keys are
pre-seeded with a decl-seed placeholder (`Any`, internally a `Package`-kind
value) for every mainline lexical before any of them run, and a plain scalar's
store only mirrors into `env` when the compiler's `needs_env_sync` analysis
(`compute_needs_env_sync` in `src/opcode.rs`) says some consumer actually
needs to read that slot by name — otherwise the slot itself is the sole
source of truth (the "(B) per-store env-write" gate). `compute_needs_env_sync`
already scans `BlockScope`/`BlockLocalScope`/`ForLoop`/`MakeGather`/
`WheneverScope` bodies to find the slots each of those constructs needs kept
live in `env`, but it never scanned `PackageScope` bodies at all — so a local
referenced ONLY inside a `module`/`package` block (or not referenced by one
at all) kept `needs_env_sync = false`, its `env` mirror was never refreshed
past the decl-seed placeholder, and the unconditional restore loop above
copied that stale placeholder straight over the live, correctly-assigned
local.

Confirmed with `rust-gdb` (breakpoints on `exec_package_scope_op`'s env
snapshots) and an instrumented trace of `needs_env_sync`/`env` per opcode:
the placeholder was visible in `env["x"]` from the very first opcode of the
program, well before `my $x` ever ran, and stayed there because nothing made
`x`'s slot `needs_env_sync`. A `class Warm { }` (or any other statement whose
own scope-exit happened to flip `needs_env_sync` to `true` for every slot as
an unrelated side effect — the `defines_lazy_body` fallback for a frame that
contains a `RegisterDecl`) coincidentally masked the bug, which is why it
looked like "only the first statement" was affected. It also meant the
*write-through* case (`my $x = 1; module M { $x = 2 }`) — which the
unconditional loop's comment claims to protect — was **itself silently
broken** whenever nothing else happened to warm `needs_env_sync` first.

## Fix

Two changes, matching the exact mechanism already used for the sibling
constructs:

1. **`compute_needs_env_sync` now scans `OpCode::PackageScope` bodies**
   (`src/opcode.rs`), feeding a new `EnvConsumerSlots::package_scope` bucket
   via the same `mark_name_access_slots`/`mark_local_access_slots`/
   `mark_same_named_slot_peers` helpers `BlockScope`/`BlockLocalScope` use.
   A slot a package/module body reads or writes (by name or by local index)
   is now correctly kept mirrored into `env`.
2. **`exec_package_scope_op`'s restore loop is gated on `code.needs_env_sync`**
   (`src/vm/vm_misc_reduction_scan.rs`): it only pulls a slot's value back out
   of `env` when the compiler says that slot's `env` mirror is actually kept
   current. A slot nothing marks (the common case — a lexical the block never
   went near) is left exactly as `run_range` left it, never consulting the
   stale placeholder.

Two other candidates were considered and rejected in favor of this one:

- **Compare `current_env[k]` against `saved_env[k]` by identity** at
  restore time, treating a change as "the block wrote it". This needs a
  cheap, genuinely-sound Rust-level identity test (not `=:=`, which is not a
  reliable identity oracle at the language level in mutsu) and is only
  correct if every write the block makes is guaranteed to reach `env` in the
  first place — which, per the root cause above, it usually is NOT (a plain
  `SetLocal` inside an inlined package body never touches `env` unless the
  slot needs it). This candidate would have "protected" a value that was
  simply stale in both snapshots, exactly the failure mode being fixed.
- **Track which keys were re-imported from `current_env` into `restored_env`**
  and refresh only those. Simpler, but "the key existed before and after" is
  not "the block wrote it" — the empty-module repro's stale placeholder
  exists in both snapshots too, so this still refreshes and the bug
  reproduces unchanged.
- **Flush every declaration eagerly so `env` is never stale** (the broader
  §1.3 dual-store campaign). Sound, but changes behavior and performance well
  beyond this one call site; the `needs_env_sync` gate reuses an existing,
  narrowly-scoped mechanism instead.

## Scope-exit boundary

| Construct | Routes through `exec_package_scope_op`? | Had this bug? |
|---|---|---|
| `module M { }` | yes (`OpCode::PackageScope`) | yes (fixed) |
| `package P { }` | yes | yes (fixed) |
| `grammar G { }` (bare package form) | yes | yes (fixed) |
| `class C { }` | no (`OpCode::RegisterDecl`, a separate lazily-compiled body) | no |
| `role R { }` | no — role bodies are not eagerly executed at declaration time at all (a raku semantic, not mutsu-specific) | no |
| bare block `{ }` | no (`OpCode::MakeHash`/`SinkPop`, not a package scope) | no |
| `BEGIN { }` | no (`OpCode::CheckPhaserStart`/`CheckPhaserEnd`) | no |
| nested `module M { module N { } }` | yes, once per nesting level | yes (fixed) |

`t/package-scope-outer-lexical-preserved.t` pins this matrix, including the
write-through case the restore loop exists for and several package/module
blocks in the same unit where only one of them touches the lexical.
`t/in-file-package-our-var.t` no longer needs its `class Warm { }` warm-up
statement, since the bug it was working around is fixed.
