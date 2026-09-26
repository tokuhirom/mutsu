# Method calls and `===` stop scaling with MRO depth and operand size

Two opcode costs flagged by the VM complexity audit (#9172) are now flat.

**`===` / `!===`** used to warm the user-`WHICH` identity of every element of
a list-shaped operand, recursively, before a comparison that only ever looks
at the container's identity. `@a === @a` therefore cost the size of the array.
The operator now warms only the parts `values_identical` actually consults: an
instance, a container's value, a mixin's base, a Pair's key and value, and a
Capture's elements. `scripts/vm-complexity-check.sh ===` went from 0.23 s at
N = 20k to 0.0016 s, flat when N doubles.

**Method calls** walked the receiver's whole MRO several times per call: the
accessor-vs-method race (`resolve_user_method_or_accessor_sym`, twice), the
winning accessor's owner, `is_native_method`, `has_user_method`, and, for a
dynamic `$o."$name"()` call, `resolve_method_with_owner`'s per-level candidate
probe (which also re-interned both names at every level). Every one of those
answers is a pure function of the registry, so they are now memoized per
`(class, method)` in `user_method_probe_memo.rs`, keyed on the registry write
generation that every registry mutation bumps. The dynamic resolver memoizes
the list of MRO levels that declare the method and walks only those.

Measured on a release build, 20k calls through a 320-deep class chain, timed
inside the program (paired runs, second run quoted):

| call | before | after |
| --- | --- | --- |
| `$o.m` | 0.18 s | 0.05 s |
| `$o."$name"()` | 0.98 s | 0.09 s |

`scripts/vm-complexity-check.sh method` at depth 160 went from 0.46 s (ratio
1.87 when the depth doubles) to 0.073 s (ratio 1.03).

Both are now independent of MRO depth (160 vs 320 levels: equal instruction
counts under callgrind).
