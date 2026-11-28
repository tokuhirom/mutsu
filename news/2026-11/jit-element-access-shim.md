# JIT-compiled loops call the element read and store fast lanes directly

`Index` and `IndexAssignLocal` are ops the JIT does not lower; it ran them through its generic
`step` shim, which calls back into the interpreter's `exec_one` and its opcode-group dispatch
before the op's own fast lane runs.

A dedicated shim, shaped like the existing one for `++` on a proven `my int`, now calls the plain
`@a[$i]` / `%h{$k}` read lane and the lean `@a[$i] = $v` store lane directly, and hands every shape
they decline to `step`. It adds no semantics of its own.

Measured with callgrind (#19286): ~100 fewer instructions per element read or store in a
JIT-compiled loop; 1.3% fewer on a tenth-size `bench-index-read`, 2.4% on `bench-threads-serial`.
