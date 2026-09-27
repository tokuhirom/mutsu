# `Class.new` skips the dispatch probe chain

`P.new(x => 1, y => 2)` on a plain class now goes straight to the native
default constructor once one call has shown that nothing else claims it
(ADR-0121 D3, #9291). The constructor itself also stopped re-deriving each
attribute's type constraint and buildability on every call.

## What changed

- **The constructor lane.** `.new` on a named receiver compiles to
  `CallMethodMut`. That opcode walked about seventy speculative probes (proto
  bodies, exception delegates, lazy lists, junction autothreading, the
  storage delegates, a failed native-method lookup, ...) before it reached
  the native default constructor. For a plain class every probe says "no" on
  every call. `src/vm/vm_ctor_lane.rs` is a cache in front of that chain: the
  constructor twin of the plain-method lane (#8880).
  - It is written only where the full chain lands in the native default
    constructor, so reaching the constructor is the proof that every probe
    declined.
  - The replay needs the same call shape as the install: method `new` on a
    type object, no modifier, not quoted, and every argument a string-keyed
    `Pair` whose value is not a `Junction`.
  - It is cleared on a registry method-generation change. An entry also
    holds the `NativeCtorPlan` it was installed with, and a replay whose
    current plan is a different `Arc` misses. The MOP mutators drop the plan
    without bumping the generation, and this check needs no call site to
    remember the lane.
  - A class with `BUILD` or `TWEAK` anywhere in its MRO, a CUnion, a class
    the program did not declare, and a class with a builtin base never enter.
- **Per-class attribute facts.** `NativeCtorPlan` now carries each
  attribute's effective type constraint (`attr_constraints`) and whether a
  named argument binds to it (`attr_buildable`).
  - The default constructor asked `attribute_type_constraint` up to three
    times per attribute per construction. Each call scanned every attribute
    and cloned a `String`, so this was O(n²) in the attribute count, even for
    an attribute the caller had provided.
  - `dispatch_bless` reads the same table.
  - `is_attribute_buildable` is still asked for an undeclared name, and only
    for that.

## Measured

Callgrind instructions per iteration of
`while $i < $n { $s = P.new(x => $i, y => 2); $i++ }`, above the same loop
with `$s = $i`. Iterations 1,010 and 3,010 differenced, so the JIT is warm in
both:

| | before | after |
|---|---:|---:|
| `P.new(x => .., y => ..)` | 17,579 | 11,580 |
| of which the `CallMethodMut` dispatch | 13,769 | 7,851 |
| of which the construction | 6,118 | 4,761 |

What remains in a construction includes:
- the bareword `P`, resolved on every call (~3,200);
- building the two `Pair` arguments (~1,900);
- the argument-source decode ahead of the lane's gate (~1,500).
