# `++nqp::atposref_i(...)` and typed arrays assigned in a closure block

Two gaps that kept Fasta's counting loop from running:

* `++nqp::atposref_i(@counts, $i)` (and `--`, postfix forms) died with "Cannot resolve
  caller prefix:<++>(Int:D)": the op hands back the element reference, but the increment
  compiled it as a plain call whose result had no container. The `atposref_*` family now
  steps what the reference holds and writes through it (a native `IntPosRef` or a shared
  array cell), calling the op once.
* `@counts = ()` as the tail of an `if` block inside a closure is an assignment used as an
  expression; it reached the captured array by name and dropped the declared `array[int]` /
  `Array[Int]` type, so the next `atposref_i` found no native array. The expression form now
  keeps the container's type as the statement form does.

Fasta `t/01-basic.rakutest` passes 10/10.
