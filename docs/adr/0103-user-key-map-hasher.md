# ADR-0103: A map keyed by *runtime data* hashes with a randomly-seeded fast hasher, not SipHash and not FxHash

- Status: Accepted (implemented)
- Date: 2026-09-13
- Issue: [#8333](https://github.com/tokuhirom/mutsu/issues/8333) (promoted out of [#7570](https://github.com/tokuhirom/mutsu/issues/7570) §2)

## Context

`HashData::map` — the table behind every Raku `%h` — was a plain
`std::collections::HashMap<String, Value>`, so it hashed with `RandomState` /
SipHash-1-3. Measured on `cc1d0284` (release, `MUTSU_JIT=off`, callgrind Ir,
40,000-iteration loops differing by exactly one `%h{$k}`):

| symbol | Ir per `%h{$k}` read |
| --- | ---: |
| `core::hash::BuildHasher::hash_one` | 90 |
| `<core::hash::sip::Hasher<S> as Hasher>::write` | 68 |
| **total SipHash** | **158** |

against a whole-read marginal cost of 1,443 Ir: **11% of an associative read**,
and the largest single component that is neither operand-read
(`exec_get_local_op_inner`, 551 Ir, [#8332](https://github.com/tokuhirom/mutsu/issues/8332))
nor interpreter dispatch (~290 Ir). Whole-program it was 5.77% of
`benchmarks/bench-hash.raku` and 7.22% of `benchmarks/hash-access.raku`.

mutsu had already declined SipHash everywhere *else*. The registry tables,
`SymMap`, `Env::tombstones`, `our_vars`, the shared-lineage store, the
compiled-functions table and the regex dynvar overlay are all `FxHashMap`, and
their doc comments all give the same reason: **the keys are program
identifiers** — class names, sub names, variable names — chosen by the source
text, never by an attacker.

`HashData::map` is the other population, and the distinction is the whole
decision. Its keys are whatever the running program put in a hash: a JSON
object's field names, a parsed header set, a CSV column. So the two objections
that kept #7570 §2 unworked are real:

1. **Iteration order is observable** through `%h.keys` / `.values` / `.pairs` /
   `.gist`, unlike the internal maps'.
2. **FxHash over attacker-chosen string keys is a collision-DoS surface**, which
   is exactly what SipHash exists to prevent. Unseeded and trivially
   invertible, it lets an attacker precompute a key set that degrades a hash to
   a linked list.

## Decision

Introduce one type for the second population and apply it uniformly:

```rust
// src/value/user_key_map.rs
pub type UserKeyState   = foldhash::fast::RandomState;
pub type UserKeyMap<V>  = HashMap<String, V, UserKeyState>;
pub type ValueMap       = UserKeyMap<Value>;
```

Every `String`-keyed map of `Value`s that is *not* one of the deliberate
`FxHashMap` program-identifier tables becomes a `ValueMap`: `HashData::map` and
`HashData::original_keys`, `BagData`/`SetData`/`MixData::original_keys`, the
`Capture` named map, `MixinOverrides`, the cold instance-attribute construction
maps, the pseudo-stash and module-scope maps, `%*ENV`.

So mutsu now has exactly **two** hasher policies, chosen by what the keys *are*:

| keys are… | hasher | why |
| --- | --- | --- |
| program identifiers (registry, env, symbols, dispatch tables) | `FxHashMap` | not attacker-supplied; iteration order is not observable |
| runtime data (`%h` contents, captures, `%*ENV`) | `ValueMap` (`foldhash::fast::RandomState`) | attacker-supplied is possible; iteration order IS observable |

### Why iteration order made this a non-event

The order objection dissolved under measurement rather than argument. std's
`RandomState` **already reseeds every process**, so mutsu's hash order is
already nondeterministic run to run:

```
$ for i in 1 2 3; do mutsu -e 'my %h = a=>1,b=>2,c=>3,d=>4; say %h.keys.join(",")'; done
c,a,d,b
b,d,a,c
a,c,b,d
```

No test could already depend on it, and Raku specifies it as arbitrary. Keeping
a **randomly seeded** hasher therefore preserves the exact observable property
mutsu already has — which is precisely what the rejected `FxHash` option would
NOT have done: it would have made the order *deterministic*, a promise mutsu
should not start making by accident. `t/collections/hash/hash-key-hashing-integrity.t`
pins the contract that does matter (the key *set*, `:exists`, delete, copy,
table growth, object-hash `original_keys`), and asserts no order at all.

### Why foldhash

- **Randomly seeded per instance**, so a colliding key set cannot be
  precomputed offline — the realistic attack this has to stop.
- It is `hashbrown`'s own default hasher, and `std::collections::HashMap` *is*
  hashbrown, so this changes the `BuildHasher` and nothing about the table
  algorithm.
- **Already in the dependency tree** (`foldhash 0.2.0`, via hashbrown), so it
  adds no new audited surface — unlike aHash, the other candidate #8333 named.
- The `fast` variant, not `quality`: the crate documents `fast` as the one
  intended for hash maps and `quality` as the one for statistical algorithms
  (HyperLogLog, MinHash).

## Consequences

Measured on the same box, release, `MUTSU_JIT=off`, callgrind Ir:

| measurement | before | after | delta |
| --- | ---: | ---: | ---: |
| SipHash Ir per `%h{$k}` read | 158 | 32 | **−80%** |
| whole marginal cost of one `%h{$k}` read | 1,443 | 1,311 | **−9.2%** |
| `bench-hash.raku` total Ir | 258.16M | 245.09M | **−5.1%** |
| `hash-access.raku` total Ir | 217.99M | 204.73M | **−6.1%** |

`__memcmp_avx2_movbe` — the equality check on the probes the hash produces, and
the thing a *worse-distributed* hasher would inflate — did not grow: 2.387M →
2.167M Ir on `bench-hash`. So the cheaper hash did not buy its speed with extra
probes.

### What this does not claim

foldhash is not cryptographic, and it does not defend against an attacker who
can observe a long-running process closely enough to recover the per-instance
seed; its own documentation disclaims both, calling itself "minimally
DoS-resistant". The threat this closes is the precomputed colliding key set fed
in as data. That is a step down from SipHash's guarantee and a large step up
from `FxHash`'s (none), and it is recorded here rather than left implicit —
which was #8333's actual requirement: *"not a silent swap"*.

If mutsu ever needs the stronger guarantee for a specific surface, the fix is a
narrower one: give that surface a SipHash-backed map, rather than paying 158 Ir
on every `%h{$k}` read in the interpreter.

## Alternatives rejected

- **FxHash, as the internal maps use.** Cheapest, and wrong here: unseeded and
  invertible over exactly the keys that can be attacker-chosen. It would also
  have made hash iteration order deterministic — a new observable promise, for
  no benefit.
- **aHash with a random seed.** Comparable guarantee to foldhash, but it is not
  in the dependency tree and foldhash is; a new direct dependency needs a reason
  beyond preference.
- **`foldhash::quality`.** One extra multiply per hash for avalanche properties
  the crate itself says a hash map does not need.
- **Keep SipHash, close #8333 as won't-fix.** Rejected: 11% of every
  associative read is not a rounding error, and the security argument for
  SipHash specifically is weaker than it looks once the seed is random either
  way.
- **A small/inline-string key representation instead.** Orthogonal, not
  alternative — some of the 158 Ir was the `String` indirection rather than the
  hash function. [`HashKey`](../../src/value/hash_key.rs) already exists for
  that and is gated by its own measurement
  ([#7549](https://github.com/tokuhirom/mutsu/issues/7549)); it composes with
  this change rather than competing with it.
