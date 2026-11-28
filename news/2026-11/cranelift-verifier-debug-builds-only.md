# Cranelift's IR verifier runs in debug builds and stress runs, and says when it fails

mutsu's JIT engine left Cranelift's `enable_verifier` setting at its default,
so every build, including the release binaries, ran the IR verifier on every
function the bytecode JIT, the sub kernels and the loop kernels compiled.
Cranelift documents the setting as a development aid ("This makes compilation
slower but catches many bugs. The verifier is always enabled by default, which
is useful during development."), and Wasmtime ships it off. In mutsu it was
about 42% of Cranelift's compile time. And it caught nothing anyone saw: every
compiler discarded the error with `.ok()?`, so a rejected function just ran on
the interpreter.

Following ADR-19368:

- the verifier runs in debug builds (`cargo test`, the gate) and is off in
  release builds;
- `MUTSU_JIT_VERIFY=1` turns it on in any build, and `MUTSU_JIT_VERIFY=0`
  turns it off;
- when it runs and rejects a generated function, the run panics with
  Cranelift's report and the function's IR;
- the nightly `jit-stress` jobs set `MUTSU_JIT_VERIFY=1`, so the release
  binary's JIT output is still verified across the whole TAP suite every night.
