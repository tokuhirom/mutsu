# The hash benchmarks have no single hot spot: the cost is spread across GC, allocation, NaN-box decode and key hashing

`todo/perf/late-august-call-path-slowdown-remainder.md` tracks a *call-path*
regression, and the three fixes that came out of it
(`news/2026-09/adr0037-routine-frame-push-intern-cost.md`,
`env-carries-its-source-file-symbol.md`,
`hot-path-well-known-symbols-and-param-bind-move.md`) recovered most of it for
`bench-fib` / `bench-tak`. They helped the hash benchmarks much less, because
those spend their time somewhere else entirely. This file records that
somewhere, so the next session does not have to re-derive it.

Measured 2026-09-03 on `731ce3cb4` + the well-known-symbol change, `perf` on a
400k-iteration variant of `benchmarks/hash-access.raku`
(`%h{"key-$_"} = $_ * 2` in a loop, then a `for %h.values` sum), JIT on,
`--profile profiling`, pinned to a P-core.

| share | symbol | cluster |
| ---: | --- | --- |
| 10.8% | `nanbox::payload_op` | NaN-box decode |
| 6.1% | `__memcmp_avx2_movbe` | string-key equality |
| 4.4% | `drop_in_place<Gc<ContainerCell>>` | GC / refcount |
| 4.3% | `malloc` | allocation |
| 3.8% | `drop_in_place<Gc<HashData>>` | GC / refcount |
| 3.6% | `__memmove_avx_unaligned_erms` | allocation / map growth |
| 3.3% | `LocalKey::with` | TLS |
| 3.0% | `gc_ptr::buffer_candidate` | GC |
| 2.7% | `_int_free` | allocation |
| 2.4% | `exec_one_dispatch` | VM dispatch |
| 2.2% | `Interpreter::current_package` | see below |
| 2.0% | `nanbox::peek::view_kind` | NaN-box decode |
| 1.9% | `Trace for ContainerCell::trace` | GC |
| 1.6% | `sip::Hasher::write` | key hashing |
| 1.5% | `Value::hash_slot_ref` | hash access |
| 1.5% | `Env::get_sym` | env |
| 1.5% | `HashMap::get` | key hashing |
| 1.3% | `gc::collect::mark_gray` | GC |
| 1.3% | `gc::collect::collect_cycles_at` | GC |
| 1.1% | `BuildHasher::hash_one` | key hashing |

Rolled up: **GC ≈ 14%** (`Gc` drops, `buffer_candidate`, `trace`, `mark_gray`,
`collect_cycles_at`, `drain_candidates`, `scan`), **allocation ≈ 12%**
(`malloc`, `_int_free`, `_int_malloc`, `cfree`, `memmove`), **key hashing +
comparison ≈ 10%** (`memcmp`, `sip::Hasher::write`, `BuildHasher::hash_one`,
`HashMap::get`), **NaN-box decode ≈ 13%**.

Nothing here is a single mistake to delete — which is exactly why it is worth
writing down rather than attacking opportunistically. Three leads, in the order
they look most tractable:

## 1. `Interpreter::current_package()` is an `RwLock` read plus a `String` clone

```rust
pub(crate) fn current_package(&self) -> String {
    self.current_package.read().unwrap().clone()
}
```

2.2% of this workload, and there are **228 call sites** across the tree. A
cheap `current_package_sym()` already exists right beside it (a relaxed atomic
load of an interned-id mirror, documented as the hot-path alternative), so the
work is mechanical: find the call sites that are on per-opcode / per-call paths
and are only comparing or formatting the name, and move them to the `Symbol`.
`vm_env_helpers.rs` alone has eight, several on variable-read fallbacks. Start
by getting a caller breakdown (`perf record --call-graph dwarf`) rather than
converting all 228 blindly — most are cold and the churn would not pay.

## 2. The Raku hash's key map hashes with SipHash

`sip::Hasher::write` + `BuildHasher::hash_one` + `HashMap::get` ≈ 4%, and the
6.1% `memcmp` is the equality check on the resulting probes. `SymMap` and the
readonly/tombstone sets already use `rustc_hash::FxHashMap` for exactly this
reason (see `Env::tombstones`' doc comment, where SipHash was ~5% of a
method-heavy benchmark).

**Do not just swap the hasher.** Unlike those internal maps, this one holds
*user* keys, so the change is observable in two ways that need deciding first:
`%h.keys` iteration order changes (Raku specifies hash order as arbitrary, but
mutsu's own `t/` may have baked the current order into expectations), and
FxHash over attacker-supplied string keys is a collision-DoS surface that
SipHash exists to avoid. Establish what the order-dependence actually is (run
the suite with a swapped hasher and read the failures) before treating this as
a perf change.

## 3. GC ≈ 14% on a workload that creates 400k `ContainerCell`s

Every `%h{...} = v` mints a container cell, and the Bacon-Rajan candidate
buffer, tracing and cycle collection follow. This is the layer ADR-0001 §7
shipped and is working as designed; the question is whether a hash element
store *needs* a cycle-collected cell in the common case where the value is a
plain scalar. That is a design question, not a tweak — it belongs in an ADR
discussion before any code.

## Method

Reproduce with the recipe in
`todo/perf/late-august-call-path-slowdown-remainder.md` ("Method notes"): solo
on an idle box, interleaved A/B, `taskset -c 0`, `-e cpu_core/cycles/u`, and
`perf` from `/usr/lib/linux-tools/6.8.0-138-generic/perf`. Any number that ends
up in a document comes from the bench CI, not from the session's local runs.
