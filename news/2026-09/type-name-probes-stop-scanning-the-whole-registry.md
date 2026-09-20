# Type-name probes stop scanning the whole registry (and re-locking it)

Second slice of [#8830](https://github.com/tokuhirom/mutsu/issues/8830), on
the costs the first one left standing. Both are the same shape as before: the
data structure could not answer a miss without visiting everything.

## `resolve_lexical_type_key` scanned every registered type, per call

ADR-0047 P1 gives a lexically-scoped `my class`/`my grammar` a mangled storage
key — `Foo\u{0}<decl-id>` — so the bare `Foo` a caller writes is not the
registry key. `resolve_lexical_type_key` bridges that, and its fallback was:

```rust
let prefix = format!("{qualified}\u{0}");
reg.classes.keys().find(|key| key.starts_with(&prefix))
    .or_else(|| reg.roles.keys().find(|key| key.starts_with(&prefix)))
    .or_else(|| reg.enum_types.keys().find(|key| key.starts_with(&prefix)))
    .or_else(|| reg.subsets.keys().find(|key| key.starts_with(&prefix)))
```

An allocation plus a linear walk over **every key in all four type maps**, on
every miss. On a `JSON::Fast.from-json` parse that was 60.3M instructions of
its own and ~125M inclusive — 4.2% of the run — for **7,236 calls**, roughly
17,000 instructions apiece. The cost scales with how many types the program
has registered, which is the wrong direction for a resolver on a hot path.

A key can only match `"{qualified}\u{0}"` when its segment before the FIRST
NUL is exactly `qualified`, so the set of those segments answers the question
exactly, in one hash probe. `Registry::has_lexical_type_key_for` is that set,
and a program that declares no lexical type at all has an empty one and never
scans again.

It is kept coherent the way `PackageKeyed`'s name filter (the first slice) is,
and for the same reason: the set lives in a `OnceLock` that
`RegistryWriteGuard`'s `DerefMut` drops, and that guard is the only path to a
`&mut Registry` in the codebase. A registration that adds a mangled key cannot
fail to invalidate it — not by convention, but because there is no other way
to reach the maps. `is_my_scoped_type_name`, which runs the same scan from
`has_type` for every package-qualified name, takes the same filter.

## `has_type_direct` took four read locks to answer one question

```rust
self.registry().classes.contains_key(name)
    || self.registry().roles.contains_key(name)
    || ...
```

Each `self.registry()` is an `RwLock` read acquisition (plus, in debug builds,
the reentrancy bookkeeping in `lock_reentry`), and the function asked for four
of them on a hit and eight on the parametric path — for four hash probes into
maps it had already reached once. `package_type_alias` alone calls it 140,460
times on this parse. Hoisting to a single guard is the whole fix.

## Measurements

`callgrind`, 100-record parse, `--profile profiling` build, against `main` at
`350b90bc` (i.e. with the first slice already landed).

| | before | after | |
| --- | ---: | ---: | ---: |
| **program total** | 2,643,952,646 | 2,579,086,749 | **-2.45%** |
| `resolve_lexical_type_key` | 60,308,580 | 412,452 | **-99.3%** |
| `__memcmp_avx2_movbe` | 99,706,471 | 49,070,398 | **-50.8%** |
| `has_type_direct` | 20,292,437 | 11,313,906 | -44.2% |
| `Interpreter::has_type` | 20,512,375 | 11,533,844 | -43.8% |

Half of all string comparison in the program was the prefix scan.

Wall clock on #8673's reproduction (727 records, release builds of the same
tree with and without the change, seven runs each): **2.398s -> 2.252s,
-6.1%**. That is more than the instruction count moved, and the two
distributions do not overlap (the slowest "after" run, 2.313s, beats the
fastest "before" one, 2.330s), so the effect is real — but a scan that touches
every key in four maps costs more than its instruction count prices, and this
box's run-to-run spread is a few percent, so treat -2.45% as the number that
is reproducible and -6.1% as this box's reading of it. `raku` on the same runs
is 0.043s.

## What this does not fix

Unchanged, and now the whole of the top: `Env::get_sym_with_fallback` (139.2M,
659,610 calls through `Env::get_sym`), `LocalKey::with` (238.0M, over a
quarter of it `Symbol::intern`), the allocator (~132M in `_int_free` alone)
and `exec_one_dispatch`. None of those are name resolution; the resolution
cluster #8830 measured has now been paid down twice and is no longer what
dominates this workload.

The issue's own claim is untouched and still stands: a lexical and a type
constraint should resolve to a slot or an id at *compile* time. Both slices
removed the cost of asking at runtime; neither stopped asking.

Pinned by two unit tests in `src/runtime/registry.rs` that drive the filter
through a real `RegistryWriteGuard` — the guard's `DerefMut` being the
invalidation mechanism, a test that used a bare `&mut` would not exercise it —
checking it against the scan it replaces on add, on a non-mangled key, on
removal, and on a multi-segment key (`Store\u{0}1::Session\u{0}2` belongs to
`Store`).
