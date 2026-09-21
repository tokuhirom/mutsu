# src/vm/ opcode handlers stop cloning values clippy proves are never read again

`clippy::redundant_clone` flagged 222 hits across 50 files under `src/vm/` (the
2026-09-20 sweep, [#8912](https://github.com/tokuhirom/mutsu/issues/8912)) —
each a `.clone()` (or an equivalent `.to_string()`/`.to_owned()`-style copy)
whose result clippy proves is never read again. Half of the raw count was
`--all-targets` double-checking the same source location once for the `lib`
target and once for the `lib test` target, so the real total was 109 distinct
call sites.

The shape is uniform across the VM's opcode dispatch handlers: build a
package/class name, a saved topic, an argument list, or a container value
once, thread it through a couple of local bindings, and clone it into the
second binding even though the first binding was never going to be read
again — a plain move does the same job for free. Since these live in the
VM's hot opcode-execution path, each removed clone is a small but real
per-opcode allocation saved.

Two of the flagged sites (`vm_exec_dispatch.rs`'s exception-unwrapping helper
and `vm_smartmatch_ops.rs`'s topic write-back) were false positives under the
default (JIT-on) feature set clippy was run against: `Value` is effectively
`Copy`-like there, so the tool couldn't see that the *other* three lint
configurations `make lint` gates on (`--no-default-features --features
native` above all) compile a `Value` shape where the "unused" binding really
is read again later in the same function. Both were left with their
`.clone()` intact rather than force a move that only compiles under one
configuration.

No behavior change — this is Rust-internal code-quality cleanup. `make test`
and `make roast` both pass (roast's four known container-sandbox failures,
per `docs/agent-environments.md`, are unrelated).
