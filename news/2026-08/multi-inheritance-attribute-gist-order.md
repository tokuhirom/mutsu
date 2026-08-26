# Attribute order in `.gist`/`.raku` now follows the MRO, not the construction order

`class Taurus is Bull is Automobile { }` rendered as
`Taurus.new(direction => Any, castrated => Bool::False)` where rakudo says
`Taurus.new(castrated => Bool::False, direction => Any)`. The divergence was not limited to multiple
inheritance and was not hash-order noise: even `class C is A { has $.c }` put the parent's
attributes first (`C.new(a => 1, a2 => 9, c => 3)` instead of `C.new(c => 3, a => 1, a2 => 9)`).

## Root cause

Rakudo has **two** attribute orders and mutsu was using one of them for both jobs:

* construction / `BUILDALL` order is base-class first, so a derived class's `BUILD` and attribute
  defaults run after the ones they may depend on;
* introspection order — what `.^attributes`, `.raku` and the default `.gist` enumerate — walks the
  MRO *forwards*, most-derived class first, each class's own attributes in declaration order.

`Interpreter::collect_class_attributes` (`src/runtime/class_introspection.rs`) implements the first:
it iterates `class_mro(...).iter().rev()` and moves a redeclared name to the end. That is correct
for the constructor plan, the `CREATE` slot allocation and `clone`, all of which use it. But
`collect_public_raku_attrs` — the single formatter behind `Class.new(attr => value, ...)` for both
`.gist` and `.raku` — used it too, so every rendering came out in construction order, i.e. reversed.
`.^attributes` was already correct, because `collect_attribute_objects` does its own forward MRO
walk; the two disagreed with each other.

## Fix

Added `collect_class_attributes_display_order`, the introspection-order twin: forward MRO walk,
first occurrence of a name wins (so a redeclared attribute keeps its most-derived position), and
pointed `collect_public_raku_attrs` at it. `collect_class_attributes` keeps its base-first
semantics for every construction-side caller, which is what those callers actually want — the two
orders are genuinely different questions, so they now have two functions.

The Pod object model in `src/runtime/runtime_init.rs` relied on the old order and repeated
`config`/`contents` on every subclass to reproduce rakudo's rendering; under the forward walk those
repeats simply dedupe against the parent's copies and the emitted order is unchanged, so the table
needed no edit — only its comment.

Pinned by `t/multi-dispatch-ordering.t` (multiple inheritance, own-before-inherited, single
inheritance, plus `.^attributes` agreement assertions so the two orders cannot drift apart again).
