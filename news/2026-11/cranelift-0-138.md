# Cranelift 0.138

Moved the JIT to cranelift 0.138.0 (wasmtime-internal crates 50 → 51), item 4 of #19210. The five
cranelift crates were bumped together. The JIT compiles against 0.138 unchanged: the release's two
IR-rejecting changes do not reach mutsu's code generator. One rejects over-aligned stack slots, the
other rejects struct arguments in the `tail` convention, and mutsu emits neither. A paired A/B on the
JIT-heavy benchmarks (`bench-fib`, `bench-tak`, `bench-mandelbrot`, `int-arith`, `num-arith`,
`bench-ripemd-rounds`) measured JIT-on instruction counts within +0.3% of 0.137, and wall-clock
differences inside the noise.
