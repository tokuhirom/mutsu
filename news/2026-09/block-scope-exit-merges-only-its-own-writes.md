# A block's scope exit merges only what the block wrote

Three scope opcodes paid, on every execution, for the size of the whole
enclosing scope rather than for what the block did:

- `BlockScope` (a bare `{ ... }` block),
- `BlockLocalScope` (an `if`/`unless`/`else` branch that declares a `my`),
- `DoBlockExpr` in its scope-isolating form (every `"...{ expr }..."`
  interpolation block).

Each saved the env on entry (`self.env().clone()`, an `Arc` bump). The block's
first by-name write then deep-copied it. On exit, each walked every visible env
entry, or collected every key, to decide what propagates. `BlockScope` and
`DoBlockExpr` also scanned all L local slots of the frame to reset or revert
them. So a script with a few thousand top-level declarations made every
`{ my $y ... }` in a loop, every `if ... { my ... }` and every `"a{ $x }b"`
proportionally slower. This is part of
[#9170](https://github.com/tokuhirom/mutsu/issues/9170).

## Block tiers

The env already had the right structure for this: a *scoped* env is an
overlay chained over a parent env, and converted call frames use it. A block
now runs over a tier of its own (`Env::open_block_tier`). It starts as an empty
overlay chained directly over the untouched enclosing env, so the block's
writes land in a map that holds only those writes. On exit,
`Env::close_block_tier` splits the tier into those writes and the enclosing
env, and the exit applies the same propagation rules as before to just those
writes (`src/vm/vm_block_env.rs`):

- `BlockScope` keeps its rules. `$!` and package-qualified names always
  propagate. A `package` declared in the block stays visible, and a `my class`
  does not. A write to an enclosing name propagates unless the block
  redeclared it, or it is the topic of a block that binds its own topic.
  Because the entry env is no longer copied, a key's membership in it is asked
  of the result env before that key's own insert.
- `BlockLocalScope` merges every write back (it has always kept a branch's
  `:=` binding to an outer variable) and removes the fresh declarations as
  before. "Did this name exist before the branch" is now asked of the untouched
  entry env for each declared name, not answered from a snapshot of every key.
- `DoBlockExpr` keeps its keep/revert table and applies it to the block's own
  writes only.

The exit identifies the tier by its parent `Arc`. If the block body replaced
the env wholesale, for example a method dispatch flattened it, the exit sees
the whole current env instead, which is what it walked before. The same
happens when the chain is already at `MAX_OVERLAY_DEPTH`. The fallback is
therefore the old behavior, not an approximation.

## Local slots

`CompiledCode` gained a lazily built name index over its locals
(`local_slots_of`, `local_slots_of_bare`, `non_plain_local_slots`):

- `BlockScope`'s exit resets the slots of the names the block declared (and
  the topic), instead of testing every slot of the frame.
- `DoBlockExpr` saves and reverts only its own declarations' slots and the
  frame's special/internal slots, not a copy of the whole `locals` vector.
- `SetVarDynamic`, which runs for every `my` inside a loop-local scope, asked
  "does this name own a slot" and "is its slot coherent with env" by scanning
  every local. Those scans were the remaining growth of the `if` branch case,
  and they are index lookups now.

## Measurements

These are release builds of `main` before and after, on the same box, each
timed inside mutsu, as `scripts/vm-complexity-check.sh` does:

| case | N | before t(N) → t(2N) | after t(N) → t(2N) |
| --- | ---: | --- | --- |
| `for ^5000 { { my $y = 1; $t += $y } }` | 500 | 0.361 → 0.653 s (1.81×) | 0.0117 → 0.0120 s (1.02×) |
| `for ^5000 { $s = "a{ $t }b" }` | 1000 | 0.380 → 0.773 s (2.04×) | 0.0047 → 0.0052 s (1.10×) |
| `for ^20000 { if $c { my $y = 1; $t += $y } }` | 2000 | 0.394 → 0.762 s | 0.031 → 0.033 s |

## Still open in #9170

- `BlockScope` still snapshots and restores the routine registry (O(R)).
  `RoutineScope`, `ImportScope`, and re-import on `use`/`import` still do the
  same.
- The O(b) declaration and topic scans of a block's ops still run on every
  entry.
- Once any sigilless alias exists in the process, the alias sync at
  `BlockScope` exit is still O(L).
- Closure creation (`capture_closure_env`) and `MakeGather` still walk or copy
  the env.

Pin: `t/control/block-scope-env-tier.t`.
