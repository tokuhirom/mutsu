# Native-typed `given`/`with` pointy param with `is rw` now writes back

`given $x -> int $v is rw { $v = 99 }` (and the `with` equivalent) silently
dropped the mutation — `$x` stayed unchanged, unlike the general
(non-native) scalar pointy-param `is rw` case fixed earlier in
`given-with-pointy-scalar-missing-readonly-enforcement.md`.

The root cause was a compiler detection gap, not a VM writeback bug.
`pointy_topic_bind`'s native-type branch (`src/parser/stmt/control.rs`)
cannot use `:=` binding for native lexicals (native lexicals can't
participate in bind mechanics at all), so it never emitted the `MarkBind`
marker the compiler's pointy-param detection (`compiler/stmt.rs`) looks for
when building the `Given`/`With` opcode's `pointy_param_idx` field. With
`pointy_param_idx` staying `None`, `exec_given_op` never learned a pointy
param existed at all — not merely that it needed writeback. A
`--dump-bytecode` comparison against the working non-native case confirmed
the native declaration already compiled to the right `SetLocalDecl` shape
the writeback machinery expects; the only missing piece was the detection.

Confirmed `given`/`with`-specific: ordinary native `is rw` sub/block
parameters already wrote back correctly (verified against a control case),
so this was not a general native-parameter-aliasing gap.

Fix: added a `__pointy_native_param` custom-trait marker to the native
branch's synthetic `VarDecl` (mirroring `MarkBind` for the general
aliasing branch), and extended `pointy_param_name` detection in
`compiler/stmt.rs` to recognize it in both shapes `pointy_topic_bind`
produces — a bare `VarDecl` for `is rw`, and a `SyntheticBlock` with a
trailing `MarkReadonly` for the readonly default.

Pin: `t/given-with-native-pointy-rw-writeback.t` (6 cases covering `given`
and `with`, `int` and `str`, unchanged-no-writeback, and in-body
mutation-chain visibility — output byte-identical to real `raku`).
PR #6334.
