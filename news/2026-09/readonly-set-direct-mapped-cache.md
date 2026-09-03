# Marking a parameter readonly stopped hashing on the recursive steady state

Every routine call marks each of its parameters read-only on entry and unmarks
them on return. In the recursive / monomorphic steady state -- the shape of any
hot call site -- that mark is a **pure no-op**: an outer frame already put the
same name in the set with the same `ReadonlyKind`, so
`mark_readonly_sym_with`'s `insert` overwrote a value with itself and its
journal arm fell through to `Some(_) => {}`. mutsu was paying a full SwissTable
*insert* (probe, write, length bookkeeping) to learn that nothing had changed:
`perf` on `bench-fib` put `mark_readonly_sym_with` at 3.0% and the
`hashbrown::HashMap::insert` it called at 3.3%.

Two changes, both on `ReadonlySet` itself:

1. **`marked_with(sym, kind)`** -- the question the mark actually wants to ask.
   `mark_readonly_sym_with` now asks it first and returns immediately when the
   answer is yes, so the common case never reaches the map at all.
2. **A direct-mapped positive cache** (`cache: [Option<(Symbol, ReadonlyKind)>;
   64]`, indexed by `sym.raw() & 63`) so `marked_with` and `contains_key` answer
   from one array load instead of a hash probe. The invariant is that an
   occupied slot `(s, k)` implies `map[s] == k`; a slot never implies *absence*,
   so any miss -- an empty slot, or one holding the symbol that evicted this
   one -- falls through to the map. That is what makes it sound under collision:
   an insert always overwrites its slot, and a remove only clears a slot that
   still names the symbol being removed.

The cache lives on the set it describes, exactly like the existing `topic`
flag, so every mutation path maintains it by construction -- including the
whole-set `mem::take` / assignment in `take_readonly_state` /
`restore_readonly_state`, which move the cache along with its map. Each read
re-derives the slow answer under `debug_assert`, and CI runs the whole `t/`
suite on a debug binary (ADR-0014), so 3600+ files check the invariant on every
push. Three unit tests pin the parts the asserts cannot reach on their own:
eviction (a colliding insert must not lose the evicted entry), removal of an
already-evicted symbol (must not clear the slot its evictor now owns), and
re-marking with a different kind.

Measured on a release build with a temporary same-binary env switch, pinned to
one core:

| benchmark | retired instructions |
| --- | ---: |
| `bench-tak` | **-3.87%** |
| `bench-fib` | **-2.67%** (cycles -5.4%) |
| `bench-class` | -0.12% |
| `poly-call` | -0.08% |
| `bench-ctor` | -0.03% |
| `method-call`, `bench-array`, `bench-hash`, `bench-string` | +0.02..0.04% |
| `bench-mandelbrot` | +0.19% |

The two recursive call benchmarks are the ones that mark parameters on every
call; everything else is within layout noise.
