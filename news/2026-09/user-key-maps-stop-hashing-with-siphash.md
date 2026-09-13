# A Raku hash stops hashing its keys with SipHash

`HashData::map` — the table behind every `%h` — was a plain
`std::collections::HashMap<String, Value>`, so it hashed with SipHash-1-3. On a
40,000-iteration loop differing by exactly one `%h{$k}` read, that was 158 Ir of
the read's 1,443 Ir marginal cost: **11% of an associative read**, and the
largest single component that is neither the two operand reads (551 Ir, #8332)
nor interpreter dispatch (~290 Ir). Whole-program it was 5.77% of
`benchmarks/bench-hash.raku`.

The interesting part of #8333 was never "swap the hasher" — it was that the
obvious swap is wrong. mutsu already uses `FxHashMap` for its registry tables,
`SymMap`, `Env::tombstones` and the compiled-functions table, and every one of
those doc comments justifies it the same way: *the keys are program
identifiers*. A Raku hash's keys are not. They are whatever the program put
there — a JSON object's field names, a parsed header set, a CSV column — so
`FxHash` (unseeded, trivially invertible) would have opened exactly the
collision-DoS hole SipHash exists to close.

The answer is a third option, now recorded as
[ADR-0103](../../docs/adr/0103-user-key-map-hasher.md): a **randomly seeded**
fast hasher. `src/value/user_key_map.rs` introduces

```rust
pub type UserKeyState  = foldhash::fast::RandomState;
pub type UserKeyMap<V> = HashMap<String, V, UserKeyState>;
pub type ValueMap      = UserKeyMap<Value>;
```

and every `String`-keyed `Value` map that holds *runtime data* moved onto it:
`HashData::map` and `original_keys`, the QuantHash original-key side maps, the
`Capture` named map, `MixinOverrides`, the cold instance-attribute construction
maps, the pseudo-stash and module-scope maps, `%*ENV`. The deliberate
`FxHashMap` program-identifier tables stayed exactly as they were, so the
interpreter now has two hasher policies and a stated rule for which one a new
map gets.

foldhash is `hashbrown`'s own default hasher, and `std::collections::HashMap`
*is* hashbrown, so this changes the `BuildHasher` and nothing about the table
algorithm. It was already in the dependency tree, so it added no new audited
surface.

## The order objection dissolved under measurement

The reason #8333 insisted the order-dependence be measured *before* any perf
decision is that a user-key map's iteration order is observable through
`%h.keys` / `.values` / `.pairs` / `.gist`. The measurement settled it in one
command: std's `RandomState` already reseeds every process, so mutsu's hash
order is **already** nondeterministic run to run.

```
$ for i in 1 2 3; do mutsu -e 'my %h = a=>1,b=>2,c=>3,d=>4; say %h.keys.join(",")'; done
c,a,d,b
b,d,a,c
a,c,b,d
```

Nothing could already depend on it. Keeping a randomly-seeded hasher therefore
preserves the exact property mutsu has today — which is the one thing `FxHash`
would have broken, by making the order *deterministic* and quietly turning an
"arbitrary" into a promise.

`t/collections/hash/hash-key-hashing-integrity.t` pins what actually matters and
asserts no order at all: every key comes back under exactly one entry, across
empty/1-byte/14/15/16/17-byte keys, multi-byte UTF-8, keys with long shared
prefixes, `:exists`, `:delete` (with its prefix siblings surviving), a hash
copy, 500 keys' worth of table growth, and an object hash's typed
`original_keys`. It passes under rakudo too.

## Results

Release build, `MUTSU_JIT=off`, callgrind Ir:

| measurement | before | after | delta |
| --- | ---: | ---: | ---: |
| SipHash Ir per `%h{$k}` read | 158 | 32 | −80% |
| whole marginal cost of one `%h{$k}` read | 1,443 | 1,311 | −9.2% |
| `bench-hash.raku` total Ir | 258.16M | 245.09M | −5.1% |
| `hash-access.raku` total Ir | 217.99M | 204.73M | −6.1% |

`__memcmp_avx2_movbe` — the equality check on whatever probes the hash produces,
and the thing a worse-distributed hasher would inflate — went *down* (2.387M →
2.167M Ir on `bench-hash`), so the cheaper hash did not buy its speed with extra
probing.

What this does not claim: foldhash is not cryptographic and does not stop an
attacker who can study a long-running process closely enough to recover the
per-instance seed. Its own docs say so. The threat it closes is the realistic
one — a precomputed colliding key set arriving as data. ADR-0103 records that
trade rather than leaving it implicit, which was #8333's actual requirement.

Wall-clock numbers for this change will come from the bench CI history, not from
this session's local runs.
