# ADR-0019 D10 follow-up: precompute role-stub and class-body-swallow flags

D10 (deleting the class/role AST registration walkers) closed by accepting
that a handful of typed registration ops still carry their raw `Stmt` as an
opaque payload for one-shot field extraction. Two of those six accepted
raw-`Stmt` reads were not payload extraction at all, though — they were a
cheap boolean *decision* about the statement's shape, re-derived every time
the op ran instead of once when it was classified. Both are now precomputed
at compile time.

`walk_role_body`'s `RoleBodyOp::Deferred` arm (`src/runtime/registration_role_decl.rs`)
used to pattern-match `raw.as_ref()` on every deferred role-body statement to
check whether it was the `__mutsu_stub_die`/`__mutsu_stub_warn` stub marker,
in order to set `cx.role_def.is_stub_role = true`. `RoleBodyOp::Deferred` now
carries an `is_stub_marker: bool` field, computed once in
`classify_role_body_stmt` (`src/opcode.rs`) via a new `is_stub_marker_stmt`
helper; `walk_role_body` just reads the flag.

`class_body_other_stmt`'s BEGIN/EVAL-swallow check
(`src/runtime/registration_class_body.rs`) similarly re-matched `stmt`'s
shape on every registration to decide whether a failure should be swallowed
(`is_swallowable`, for `BEGIN`/`EVAL` statements) and whether a `has`
declaration executed by the statement should attach to the class currently
being defined (`is_compile_time_phaser`, for `BEGIN`/`CHECK` phasers).
`ClassBodyOp::Other` and `ClassBodyOp::ClassSub` (`src/opcode.rs`) now carry
both booleans, computed once by `classify_class_body_stmt` via the new
`is_swallowable_class_body_stmt`/`is_compile_time_phaser_stmt` helpers; the
registration-time function takes them as plain `bool` parameters instead of
re-deriving them from `raw`.

Both variants already boxed/owned their raw `Stmt` per D10's accepted shape,
so the extra booleans are a negligible size increase — the `opcode_size_guard`
test (`size_of::<OpCode>() <= 48`) still passes unchanged. This is pure
architecture hardening: no behavior change, verified by the existing
`compiler::declaration_plan_tests` unit-test suite (22 tests) and the local
`t/role*.t`/`t/class*.t` prove suites (84 files, 692 tests), all still green.
