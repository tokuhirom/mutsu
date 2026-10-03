# NativeCall's rung-3 exception is re-decided: upstream runs via its backend-neutral path

ADR-0096 kept `NativeCall` as its last justified native provider on the strength of two
structural blockers: upstream builds each `is native` body as a QAST tree, and its dispatcher
is written against MoarVM's dispatch programs. Re-reading rakudo 2026.06 and 2026.09 showed
that neither blocker is real. The `use QAST:from<NQP>` on line 2 is the only mention of QAST in
the five files. `NativeCall::Dispatcher` is only `require`d when the compiler
`supports-op('dispatch_v')`. Otherwise upstream binds a closure that calls
`nqp::nativecall`, which is the path mutsu already selects.

ADR-11203 therefore supersedes ADR-0096 §D4/E1. Upstream `NativeCall` will be vendored
verbatim, with no patch. The six FFI `nqp::` ops become the VM layer that the existing Rust
machinery is re-homed behind. The remaining gaps are 14 `nqp::` ops, native-type semantics, an
anonymous `multi` term, `Code.$!do`, and REPRs selected by `is repr<...>`. They are filed as
#11204–#11211 under #11203. The native provider stays until the vendored module passes every
bundled-library suite.
