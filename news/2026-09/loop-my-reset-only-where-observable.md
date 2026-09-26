# A loop body's `my` skips its per-iteration reset where nothing can see it

`benchmarks/time-parts.raku` and `time-parts+jit` stepped up by +7.2% and
+12.5% instructions in the deterministic benchmark history (#9537). The step
landed on the #9428 merge, but the CI run before it was six merges earlier, and
a callgrind A/B of #9428's own parents showed no change. The cause was #9426
("reset failed declaration initializers"): every ordinary `my` in a loop body
now reset its reused binding before its initializer ran -- a local-slot store
plus an env `insert_sym` per declaration per iteration, five times per
iteration of `time-parts`' loop.

That reset is only observable by code that can read the name while the
initializer runs or after it aborted: a CATCH/CONTROL handler or phaser of the
body, a closure or routine declared in the body, `EVAL`, or a dynamic
variable's callees. After compiling a loop body the compiler now checks for
all of those -- the body as written has no CATCH, CONTROL or phaser, no
`Compiler` was created while compiling it (so it holds no code object), and
every opcode emitted for it is on a small allowlist of constant, local,
arithmetic, comparison and jump ops -- and only then downgrades its
non-dynamic declarations to `DeclReset::SeedIfUnbound`, the pre-#9426
behavior. Anything the check cannot prove keeps the reset, so a missed case
costs speed, never correctness.

`SetVarDynamic`'s `reset_binding: bool` became the three-way `DeclReset`
(`Keep` / `SeedIfUnbound` / `Fresh`). `t/exceptions/loop-declaration-reset-observers.t`
pins the observers that must still see a fresh binding, and unit tests in
`src/compiler/decl_reset.rs` pin which bodies qualify.
