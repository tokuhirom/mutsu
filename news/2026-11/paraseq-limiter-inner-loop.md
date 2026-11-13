# Cheaper nqp-style inner loops: IterationBuffer ops, captured `++`, enum stores

ParaSeq's `max`/`min` limiter (#17337) runs a tight `nqp::while` loop per element. The loop does
`nqp::atpos`, `nqp::push` and `nqp::setelems` on an `IterationBuffer`, `++` on a `uint` that the
cued block captures, and `$cmp = by($a, $b)` into a `my int`. On the stand-alone model of that
loop, the instruction count per element dropped by about a quarter (909M -> 694M Ir for 20,000
elements). ParaSeq's `t/30-max` went from 14 s to 9 s and `t/31-min` from 13 s to 9 s on a
4-core box.

- `nqp::push`/`shift`/... no longer clone the operand just to find out it is not a
  `ConcBlockingQueue`. The queue probe now looks through an argument wrapper or a scalar
  container by reference.
- `nqp::atpos`, `nqp::push` and the new `nqp::setelems` lane on a plain `IterationBuffer` reach
  its storage directly. They skip the high-level-Array refusal, the Buf probe and the
  storage-handle clone, which the general forms need only for other operands. `setelems`'s fill
  rule is one function (`nqp_backing::setelems_items`) shared by both paths.
- `++`/`--` on a captured variable (a closure cell) reads the cell's own type before falling back
  to the env lookup by name. An `Int` stepped inside a native-int or `Int` constraint skips the
  general type check, since the wrap already keeps it in range.
- Assigning an `Order` or `Bool` to a `my int` unboxes it to its integer and then takes the plain
  scalar-store lane. The typed store produces the same value; it just reached it on the slow
  path.
- A read of a captured variable takes the cell once instead of twice.

Filed on the way: #17662 (`nqp::isnull` is false for a grown `IterationBuffer` slot).
