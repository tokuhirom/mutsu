# Attributes live in per-class slots, and a `$!x` site remembers its slot

[ADR-0121](../../docs/adr/0121-instance-attributes-live-in-per-class-slots.md) D2 and the
first part of D3 (#9291). This follows the two D1 slices
(`attribute-access-resolves-owner-and-self-once-per-frame.md`,
`nqp-getattr-bindattr-create-stop-copying-the-object.md`).

## D2: `ClassLayout` and slot storage

- **`ClassLayout`.** A composed class now has one. It records every declared attribute's
  storage key, in MRO order with parents first, plus the key -> slot index and a unique
  `id`. It is built once per class from the constructor plan (`NativeCtorPlan::layout`)
  and is invalidated wherever the plan is. Instances that already exist keep the layout
  they were built with, which stays valid for them.
- **`AttrMap` stores slots.** It holds `slots: Vec<Option<Value>>`, one per layout key
  (`None` = absent), plus an `extra` map for anything the layout does not declare: the
  attributes of a builtin base such as `Exception`, and internal markers. A map with no
  layout keeps everything in `extra`, exactly as before.
- **The key-based API is unchanged.** Not one of the ~400 call sites moved.
- **Construction.** The native default constructor, `bless` and the `CREATE` template
  build their maps over the class's layout.
- **Iteration order** is now declaration order (slots first, then `extra`) instead of
  hash order. That is also what rakudo shows for `.raku` and `.^attributes`.

## D3 (first part): per-site cache for `$!x` / `$.x` in a method

- **The cache.** Each local slot of a chunk has one cache word, `layout_id << 32 | slot`
  (`CompiledCode::attr_sites`). It is filled only from the general resolution, and only
  when that resolution's choice of key cannot vary for other accesses on the same layout
  (`ClassLayout::site_cacheable_slot`).
- **Not cached: owner-dependent keys.** A private attribute declared in both a parent and
  a child, or a sigil-colliding one, is never cached, because which key wins depends on
  the running method's owner.
- **Not cached: a winner that could change later.** If a higher-ranked candidate key is a
  declared slot that is merely absent right now, it is not cached, since a later fill of
  that slot would win.
- **Conditions for a hit:**
  - the instance has the same layout;
  - its `extra` is empty, since an undeclared key there could outrank the slot;
  - the slot is present;
  - the running method's owner is not a role, since a role method on a mixin resolves
    against the role's own cell first.
- **What a hit does.** A read is one atomic load plus a slot read under the read guard.
  A write stores into the slot under the write lock, through a `:=`-bound container
  exactly as the keyed store does, and still records the BUILD-phase write.
- **On a miss** the general resolution runs and refills the cache.

## Measured

Callgrind, 1 vs 10,001 iterations of a method loop, per access above the empty loop.
"Before" is `main` ahead of the first D1 slice:

| | before D1 | after D1 | after D2/D3 |
| --- | ---: | ---: | ---: |
| `$s = $!x` | 4,166 Ir | 3,302 Ir | 2,976 Ir |
| `$!y = $i` | 10,727 Ir | 7,985 Ir | 7,466 Ir |

The `GetLocal` of `$!x` as a whole went from 1,134 to 783 instructions. Its attribute part
is now a cache hit of about 90.

Wall clock, release build, `tmp/attr-bench.raku`-style, 200,000 iterations: a read went
from ~684 to ~535 ns per iteration, and a write from ~1,284 to ~960.

What each iteration still pays is mostly not attribute access:

- on a read, the general `SetLocal` cascade that stores the value into `$s` (~1,480);
- on a write, the attribute's declared-type lookup (~2,365), plus that same cascade.

Still open from D3:
- the generated-accessor fast path;
- caches at constant `getattr` / `bindattr` sites;
- real `$!descriptor` answers on `Array` / `Hash`.

D4 (`nqp::attrinited`) is also still open.

Pinned by `t/oo/attribute/attr-slot-site-cache.t`:
- one method site across several layouts;
- owner-dependent private attributes;
- a builtin base;
- a mixin;
- `augment`;
- declaration order;
- `eqv` and clones;
- a bound container.
