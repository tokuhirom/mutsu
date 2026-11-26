# ADR-19206: Built-in classes are a static catalogue, materialized on first lookup

- **Status**: Accepted (by the maintainer, 2026-11-26); slice 1 landed
- **Date**: 2026-11-26
- **Carrier issue**: #19206, a sub-issue of #19197 (start-up is slower than Raku++ in kernel
  time and instructions)
- **Related**: ADR-17216 (one program world, shared by every thread; the registry's
  copy-on-write tables), ADR-0029 (the `X::` exception classes' parents and composed roles),
  #18956 (built-in exception attributes declared on first access)

## 1. Context

`Interpreter::new` builds the whole built-in declaration registry before the program runs.
`TypeState::new` calls `Interpreter::shared_builtin_registry`, which runs
`build_builtin_registry` once per process. The result does not depend on the program, the
environment or the arguments: it is a constant of the binary. Rebuilding it on every start
costs the following for `benchmarks/bench-startup.raku` (`say "hello"`):

| cost | amount | share |
|---|---:|---:|
| `build_builtin_registry`, instructions | 3.56M of 7.75M | 46% |
| ... the `register_x` closure (~367 `X::` classes) | 1.77M | 23% |
| ... `Symbol::intern` (MRO names, mostly) | 1.36M | 17.5% |
| `ClassDef` heap (~450 × ~530 B) | 238 KB | ~58 pages, one fault each |
| the builder's own text | ~3,000 lines of straight-line code | executed once, faulted in every run |

Measured with callgrind (`MUTSU_JIT=off`) and massif on `main` at `b2cdb3da` plus #19204.

The program uses a handful of these classes. #18956 already moved one part of this cost to
first use, the attributes of built-in exception classes, and halved start-up. This ADR applies
the same rule to the classes themselves.

`Registry::classes` (`ClassTable`) is a copy-on-write sharded map (ADR-17216). It has ~450 call
sites:

| method | call sites |
|---|---:|
| `contains_key` | 218 |
| `get` | 105 |
| `insert` | 33 |
| `keys` | 32 |
| `get_mut` | 19 |
| `remove` | 7 |
| whole-table uses: snapshot `clone`, `retain` and refill on EVAL rollback, `shares_with`, `len` / `key_generation` for the type-name indexes, iteration on `require` | 26 |

## 2. Decision

### 2.1 A static catalogue under the copy-on-write table

`ClassTable` becomes a two-layer table with the same interface:

- **Catalogue (bottom).** A static, read-only description of built-in classes, compiled into
  the binary. It holds, per class, what is needed to build its `ClassDef` (name, parent, composed
  roles, and per family whatever else is constant). It has a process-wide
  `OnceLock<CowTable<ClassDef>>` slot per row.
- **Overlay (top).** Today's `Sharded<String, CowTable<ClassDef>>`. It holds user classes,
  built-ins not (yet) in the catalogue, and copies of catalogue classes that have been written
  to.

The operations:

- `contains_key(name)`: checks the overlay, then the catalogue (by name, without
  materializing), minus hidden names.
- `get(name)`: checks the overlay first. If the name is a visible catalogue row, it initializes
  that row's `OnceLock` once per process and returns the shared `CowTable`. That lookup is a
  binary search over the row names and the `OnceLock` is a static, so a hit after the first one
  costs no allocation.
- `get_mut(name)`: if the class is only in the catalogue, `get_mut` first inserts a clone of
  the materialized `CowTable` into the overlay. Cloning a `CowTable` is a reference-count bump.
  It then mutates the overlay entry as today. The catalogue is never written.
- `insert(name, def)`: writes the overlay. It shadows a catalogue row of the same name.
- `remove(name)`: removes the overlay entry and hides the catalogue row. The set of hidden rows
  is a small copy-on-write set, so snapshots and EVAL rollback (`retain_names`, `refill_from`)
  restore it along with the rest.
- `keys()`: the overlay's keys, then the visible catalogue names that the overlay does not
  shadow. Keys stay `&String`, so no call site changes. The catalogue's names are built as
  `String`s once per process, on the first walk.
- `key_generation()`: also moves when the hidden set changes, so the type-name indexes
  (`type_keys_stamp`, `class_key_by_short_name`, the mixin display index) stay coherent. Each
  half is a process-unique registry generation, so the larger of the two moves whenever
  either does. A size check (`len()`) is not offered, because it would cost a pass over the
  catalogue. The one index that compared sizes now compares key generations.
- Whole-table iteration materializes every visible row. It is allowed, but must not happen on
  the start-up path. `build_builtin_registry` reads only the overlay, and the composed-role
  registries read the catalogue rows directly. A callgrind profile of start-up shows no
  materialization.

A materialized `ClassDef` is the same value the eager builder produced. Its parents and MRO come
from the catalogue rows. Its built-in exception attributes are marked for deferral (#18956), as
`BuiltinClassTable::insert` does today. A unit test materializes every row and compares it with
what the eager code built. Each slice that moves a family into the catalogue keeps that
comparison for the family until the eager code is gone.

### 2.2 The catalogue adds no relocations

The catalogue is data in `.rodata`: one `&'static str` of newline-separated rows, plus
`const`-evaluated `u32` arrays of row offsets, sorted by name and checked at compile time. A
table of `&'static str` (one relocation per string) would add copy-on-write pages to
`.data.rel.ro` at every start, which is #19197's other large cost. So would a `phf` map of
`&str` keys.

### 2.3 Slicing

1. The `register_x` classes: ~367 `X::` exception classes, each a name, one parent and a list of
   composed roles (ADR-0029). The class rows move to the catalogue. The composed-role registries
   that `register_x` fills stay eager in this slice; they are read from the catalogue rows.
2. The remaining built-in classes, by family, as far as their `ClassDef`s are constant.
3. The other maps `build_builtin_registry` fills eagerly: the composed-role registries, the
   accessor rows and the seeded enum types.
4. Built-in names as pre-interned `Symbol`s, so the names a materialized class needs cost no
   `intern` allocation.

The same rule, nothing constant is built before it is used, applies to the rest of
`Interpreter::new`. Those parts are tracked as their own issues: `%*ENV`, which parses every
environment variable for an allomorph (10.5% of start-up instructions), and the module
repository wiring (~6%).

## 3. Consequences

- Start-up stops paying for built-ins the program never names. The cost moves to the first
  lookup of each class: one `OnceLock` initialization and one `ClassDef` per class, once per
  process.
- The `OnceLock` slots are shared by every interpreter and thread in the process. A
  materialized `ClassDef` is immutable there, and writes go to the overlay. This is the same
  rule as ADR-17216's copy-on-write world, one level down.
- The catalogue's contents are maintained as data, not code: the `X::` rows stay generated
  from `docs/data/x-exception-role-membership.tsv`, per ADR-0029.

## 4. Alternatives considered

- **Keep building eagerly, but faster** (smaller `ClassDef`, cheaper interning). Each such
  change trims a constant. None removes the work, and together they cannot reach #19197's
  goal.
- **Build the registry at compile time** (a `build.rs`-generated `Registry` value). A
  `ClassDef` holds `HashMap`s, `Arc`s and `String`s, which are not `const`-constructible. This
  would still need a run-time deserialization of every class, which is the eager cost again.
- **Snapshot the built registry into the binary or a cache file and map it.** It is the same
  deserialization problem, plus a cache-invalidation and trust-boundary question
  (docs/security.md: runtime-written files).
- **Link non-PIE** to drop the relocation faults instead. This was rejected by the maintainer:
  it weakens ASLR (#11158).

## 5. Open questions

- Which whole-table iterations run for ordinary programs (not at start-up, but early)? Each
  forces every visible row to materialize. Slice 1 measures this, and if it happens, the
  iteration is rewritten to need only names.
- Whether `keys()` should yield the catalogue in sorted order, which would give the type-name
  indexes a deterministic order for free.
