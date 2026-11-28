# The JIT compiles loops that read a captured array through `IndexUpvalue`

ADR-18637 fused a captured array's element read (`GetUpvalue` then `Index`) into one
`IndexUpvalue` opcode, so a `start` block reading a shared `:=`-bound array could take
the lock-free snapshot path. The Tier A JIT's step list never learned the new opcode,
so any range containing it was declined as a whole and ran on the interpreter. The
reader loop of `bench-threads` (`$s += @alias[$i +& 255] + $sref`) was one such range:
`MUTSU_VM_STATS` reported `jit bailout opcodes: IndexUpvalue=1` (#19406).

`IndexUpvalue` is now stepped like its two halves. The step helper runs the very same
`exec_one` arm, so the snapshot read and its soundness argument (ADR-18637 §6.1) are
unchanged; only the surrounding arithmetic, loop control and local stores stop going
through the interpreter. `t/vm/codegen/jit-index-upvalue-step.t` pins the JIT-on and
JIT-off transcripts of readers, mixed readers/writers and non-integer subscripts
against each other and against rakudo's answers.
