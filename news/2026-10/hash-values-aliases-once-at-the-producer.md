# `for %h.values` aliases each element once, at the producer

`hash-access+jit` was still 18% over its August baseline after the late-August call-path work
(#7579) finished. Splitting the benchmark into pieces showed that half of it was the second
loop, `for %h.values { $sum += $_ }`, and most of that came before the body ran: an **empty**
`for %h.values { }` cost about 5,940 instructions per element. An empty `for ^10000 { }` costs
about 430.

The implicit topic of `for %h.values` aliases each hash element (ADR-0045), and two layers did
the full aliasing work for every element:

- the container-aware `.values` producer copied every key into a `Vec<String>`, then called
  `hash_slot_ref(key)` per key: a fresh hash probe, a fresh `String` for the owner name, and
  the cell promotion;
- the loop then copied every key a second time for a writeback it would never run. Per
  iteration it looked the source hash up by name, probed `hash_get_str(key)` and called
  `hash_slot_ref(key)` again, all to find the cell the producer had already put in the item.

The bind site already treated an item that is an element cell as aliased and retired its
writeback, and `@a.values` already skipped the loop-side plan for that reason. `%h.values` now
does the same: when every item is an element cell, the loop captures no keys and plans no
`HashValue` alias. The producer promotes the whole map in one pass with the new
`Value::hash_element_cells`, so no key is copied, re-hashed or re-probed. `hash_slot_ref` moved
with it into `src/value/hash_slot.rs`, and the single-key and bulk forms share one promotion
routine.

Callgrind, warm, `--profile profiling`:

| | before | after |
| --- | ---: | ---: |
| `benchmarks/hash-access.raku` | 178,725,597 Ir | 143,497,250 Ir (**-19.7%**) |
| empty `for %h.values { }`, per element | ~5,940 Ir | ~2,420 Ir |

`t/collections/hash/hash-values-producer-alias.t` pins the aliasing edge cases that the removed
per-iteration re-lookup used to decide: a second loop over cells that are already promoted, a
body that reassigns or deletes from the hash, the typed-hash error naming `%h`, and
`.values`/`.kv`/`.pairs` following `.keys` order.

What remains of the values loop is the generic per-iteration cost of `for`, which array loops
pay too. The other half of the benchmark is the insert loop, where `"key-$_"` interpolation costs
about 3,300 instructions per iteration (#10951).
