# Shifting an array's first element is amortized O(1)

`nqp::shift` / `nqp::shift_i` and `Array.shift` removed the first element with
`Vec::remove(0)`, a memmove of the whole remaining array, so consuming a list
from the front was quadratic in its length. MoarVM's `VMArray` keeps a start
offset and shifts in O(1). JSON::Fast's `unjsonify-string` is exactly that
loop -- it `nqp::shift_i`s every codepoint off a `Uni` -- so every JSON string
containing a backslash escape paid it, and at 4000 characters callgrind put
53% of a whole `from-json` run in `memcpy` under `nqp_shift` (#9121).

`ArrayData` now carries a `head` offset. `ArrayData::shift_front` overwrites
the front slot with `Nil` (so the dead prefix keeps nothing alive) and
advances `head`; the dead prefix is dropped once it outgrows the live part, so
the compaction memmove is paid at most once per element shifted. `Deref` now
targets the live slice (`[Value]`) instead of `&Vec<Value>`, `items_mut()` /
`take_items()` / `into_items()` / native promotion compact first, and the
`Vec` mutators callers relied on through `DerefMut` (`push`, `pop`, `extend`,
`insert`, `remove`, ...) are forwarded explicitly -- `remove(0)` routes to
`shift_front`, which is how the `Array.shift` method paths pick it up. `Clone`
copies only the live elements. A native-backed (ADR-0030) array never carries
a head offset; its shift keeps the old path.

## Measured

`from-json` of 400,000 characters split into escaped strings of length L
(the issue's repro), release build, second run of each, paired A/B on one
4-core container:

| workload | before | after |
| --- | ---: | ---: |
| `from-json`, L = 2000 | 330 ns/char | 268 ns/char |
| `from-json`, L = 8000 | 852 ns/char | 365 ns/char |
| `from-json`, L = 32000 | 3127 ns/char | 831 ns/char |
| `from-json`, L = 100000 | 9741 ns/char | 2148 ns/char |
| `nqp::shift_i` drain of a 100k `Uni` | 8399 ns/elem | 599 ns/elem |
| `@a.shift while @a`, 100k elements | 8244 ns/elem | 470 ns/elem |

The shift loop itself is now flat in its length. `from-json` still grows
with L after this change, but the remainder is not the shift: the same
`shift_i`/`push_i` loop run inside a sub whose closure captures the `Uni`
(as `unjsonify-string`'s `fetch-codepoint` does) is linear at ~2000
ns/elem, so what is left belongs to the parent tracking issue (#8673).

Pinned by `t/collections/array-shift-is-constant-time.t`, which checks that
the offset is invisible to indexing, iteration, `push`/`unshift`/`pop`,
`eqv` and `.clone`, and that draining a 4x longer `Uni` with `nqp::shift_i`
costs ~4x rather than ~16x.
