# Cranelift 0.137

Moved the JIT to cranelift 0.137.1. The five cranelift crates must be bumped together (a partial bump
leaves two `cranelift_codegen` versions in the graph), and `MaybeRelSourceLoc::bits` is gone, so the JIT
dump reads the op index through `as_abs()`.
