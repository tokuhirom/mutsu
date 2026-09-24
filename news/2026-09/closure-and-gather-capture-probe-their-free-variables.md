# Closure and gather capture look up their free variables instead of walking the scope

Creating a closure (`MakeAnonSub`, `MakeAnonSubParams`, `MakeLambda`,
`MakeBlockClosure`) and evaluating a `gather` both cost time in proportion to
the number of declarations in the enclosing scope, not to what the body
references. In a file with a few thousand top-level `my`s, every `-> { ... }`
and every `gather` in a loop got slower in step with the file. This is part of
[#9170](https://github.com/tokuhirom/mutsu/issues/9170); the block-scope half
landed earlier (`block-scope-exit-merges-only-its-own-writes.md`).

## Where the time went

- **The capture walk.** `capture_closure_env` builds the closure's env by walking
  every env tier in reach (`Env::filtered_flat_capture`) and keeping the entries
  its filter accepts. A tier's key-set memo (#7565) already skipped the keys no
  capture ever keeps, but every plain user lexical (a lowercase `my` name) was
  still a candidate, because the filter keeps one when it is a free variable of
  the closure. So the walk visited every declaration in the file to keep the
  one or two the body named.
- **`gather`** cloned the whole env and then inserted its bookkeeping keys, which
  copied the creating scope's overlay on the first insert.
- **Three O(L) scans over the frame's locals** on the same paths:
  `resolve_capture_slot`'s fallback `rposition` name search,
  `box_captured_lexicals`' `dup_named_locals.iter().any(..)` (shadow slots are on
  by default, so this ran on every creation), and `find_local_slot`'s `position`
  search, which every bare call in the frame reaches.

## The change

The capture filter's verdict on a plain user lexical is exactly "is it a free
variable". So the tier memo now leaves plain user lexicals out of its candidate
list, together with their `__mutsu_type::` shadows, which the filter decides by
their subject (`env_tier::capture_walk_skips`). The capture then probes the
closure's free variables, and their type shadows, by name in each tier
(`CompiledCode::capture_probe_keys`, built once per chunk). The filter still
decides every probed key, and the walk's shadowing and tombstone handling is
unchanged, so the result is the same map as before.

`MakeGather` now takes the same filtered capture a closure literal in its
place would. The body's free variables are already known (the analysis chunk
`box_captured_lexicals` uses), and the pull path already runs the body over a
scoped child of that env.

The three scans now use the chunk's local name index (`local_slots_of`, from the
block-scope slice) or a flag recorded when `dup_named_locals` is built.
`find_local_slot` looks the name up with `Symbol::lookup` rather than
interning it, which keeps `tests/named_call_intern_budget.rs` within budget.

## Measured

Release build, second run of each, 20000 iterations, N top-level `my`
declarations in front (`tmp/` repros, same shapes as the new
`scripts/vm-complexity-check.sh` cases):

| body | before, N = 0 / 2000 / 8000 | after, N = 0 / 2000 / 8000 |
|---|---|---|
| `my $q = $_; my $c = -> { $q }` | 0.041 / 0.39 / 1.53 s | 0.041 / 0.041 / 0.042 s |
| `my @g = gather { take 1 }` | 0.091 / 0.33 / 1.13 s | 0.081 / 0.083 / 0.093 s |
| `sub mk { my $q = 1; -> { $q } }; mk()()` | — / 0.51 / — s (0.77 at 4000) | 0.147 / 0.146 / 0.151 s |

The pin is `t/routines/closure/closure-capture-probes-plain-lexicals.t`. It
uses a wide file scope and loops so the memoized path is actually taken, and
checks plain, typed, `@`/`%`, late-declared and shadowed lexicals, closures
created inside a sub, and `gather` reading and writing outer lexicals.

## Still open in #9170

Routine-registry snapshot/restore (`RoutineScope`, `ImportScope`,
`PushImportScope`/`PopImportScope`, `BlockScope`'s `scope_routines`, re-import),
the O(b) op scans on block entry, and the O(L) sigilless-alias sync at
`BlockScope` exit. The capture keeps O(s) in the scope's *system* names (types,
specials, `__mutsu_` metadata), so the `Rakudo: O(f)` suffixes stay on the
closure and gather arms.
