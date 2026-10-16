# Any's iteration methods are rows found by the receiver's MRO

`map`, `grep`, `first`, `reduce`, `produce`, `iterator`, `eager`, `squish`, `rotor`, `skip`, `collate`,
`classify`, `categorize`, `classify-list` and `categorize-list` lost their by-name arms in the interpreter's method
cascades: a call the method table's entry does not answer (an instance, a type object) now walks the receiver's MRO
to `Any`'s row, as `.^can` finds it (ADR-11276 §9.134). The rows used to decline user-iterator, `IterationBuffer` and
lazy-pipe receivers because the VM asks the table before the interpreter's receiver probes run; that is now the VM
entry's own veto, so a row is the method wherever the probes have already run. A call no candidate takes
(`(1, 2, 3).reduce()`, `.squish(1)`) is `X::Multi::NoMatch`, as in Rakudo.
