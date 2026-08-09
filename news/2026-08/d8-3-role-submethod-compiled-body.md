# ADR-0019 D8-3: role BUILD/TWEAK submethods run precompiled bytecode when mixed onto a plain value

When `$value does Role` or `$value but Role` composes a role onto a plain, non-`Instance` value —
an `Int`, `Str`, or any other scalar, as opposed to `class C does Role` construction, which already
had its own compiled path — the role's `BUILD`/`TWEAK` submethods ran through
`run_role_submethod` (`src/runtime/types/roles.rs`). That function re-parsed and re-compiled the
submethod's raw AST body via the `eval_block_value` carrier on every single composition, instead of
reusing the bytecode chunk the role's method registration already compiled once
(`MethodDef::compiled_code`).

`run_role_submethod` now runs that precompiled chunk directly via `run_compiled_block_raw`, falling
back to `eval_block_value` only for the rare method that has no compiled chunk (e.g. one installed
through a meta-programming hook). This closes the `run_role_submethod` rider mentioned in ADR-0019's
D8 slice — the compile-once-per-role-declaration principle D8-1/D8-2 already established for a
role's other deferred body statements now covers its BUILD/TWEAK submethods too.

Behavior is unchanged: `$!attr` reads/writes inside such a submethod body were already resolved
through plain env keys (`run_role_submethod` seeds/reads back `env["!attr_name"]` itself, since
`self` here is a `Mixin` wrapping a non-`Instance` value with no attribute cell to route through) —
the compiled `GetLocal`/`SetLocal` ops' cell-lookup machinery simply no-ops for this shape of `self`
in both directions, and execution falls through to the ordinary VM local slot, which `run_nested`
bridges to/from `env` at frame entry/exit exactly as the old AST-walking path did. Verified with a
raku-checked repro (scalar-attribute `BUILD`/`TWEAK`, ordering, captured-outer-lexical writeback, the
non-mutating `but` form), pinned by `t/role-submethod-runtime-does-compiled.t`, the full `t/` suite,
and every whitelisted `S06-signature`/`S12-*`/`S14-*` roast file.

Verification also turned up two pre-existing bugs in this same composition path, confirmed identical
before and after this change (so not regressions, and out of scope for this slice): an `@!attr`/
`%!attr` write inside such a submethod silently drops (`todo/tickets/role-submethod-array-hash-attr-key-mismatch.md`),
and a parameterized role's own type/value parameter is invisible inside its `BUILD`/`TWEAK` when
composed this way (`todo/tickets/role-submethod-runtime-does-parameterized-value.md`).
