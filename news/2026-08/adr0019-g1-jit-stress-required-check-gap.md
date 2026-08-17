# ADR-0019 G1 closed: fixed a jit-stress required-check gap

ADR-0019's completion gate G1 ("full compatibility gate") asks whether `make test`, `make roast`,
GC stress, JIT stress, WASM, and the bundled-library suites all pass with no new quarantine. Read
literally, this is not a one-time run to perform: it is exactly what the `main` branch ruleset's
required status checks already enforce on every merge, since branch protection only lets a green PR
reach `main`.

Verifying that claim against the actual ruleset (`gh api repos/tokuhirom/mutsu/rulesets/12935729`)
found a real gap: the `jit-stress` CI job runs the full `t/`+roast suite with the Cranelift JIT
forced hot on every PR, and its own comment in `ci.yml` calls every step a "BLOCKING gate" — but it
was missing from `required_status_checks` (which only listed `test`, `wasm-e2e`, `gc-stress`,
`changes`, and `miri`). A failing `jit-stress` run would not actually have blocked auto-merge.

Fixed by adding `jit-stress` to the ruleset's required status checks, so a JIT codegen/shim
regression can no longer merge to `main` unnoticed. This closes ADR-0019's G1.
