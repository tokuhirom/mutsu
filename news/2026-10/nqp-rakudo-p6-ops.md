# Sixteen of Rakudo's `p6*` nqp ops

`use nqp; say nqp::p6definite(42)` died with `Unsupported nqp:: op`. The
`p6*` ops are Rakudo-only: they are registered for the Raku HLL in Rakudo's
`src/vm/moar/Perl6/Ops.nqp`, not in NQP's op reference. Sixteen of the
twenty-five that were missing now work (#11505), which takes the category from
1 / 26 to 17 / 26 in `docs/nqp-op-coverage.md`.

The value ops `p6definite`, `p6box`, `p6decontrv`, `p6decontrv_6c`,
`p6typecheckrv`, `p6bindassert`, `p6capturelex`, `p6getouterctx` and
`p6setautothreader` form a new link of the `nqp::` table chain
(`runtime/nqp_ops_p6.rs`). Each one reuses the routine mutsu already has for
the job: the concreteness test, the return-type check a `return` takes, the
binding type check, and the context snapshot `nqp::ctx` makes.

The binder ops `p6isbindable`, `p6bindcaptosig` and `p6trialbind` take a
`Signature` value. A signature built from a declaration now keeps the
declared parameters as well as its introspection view (`SigInfo::param_defs`).
That lets these ops run mutsu's one signature binder, so a failed
`p6bindcaptosig` raises the same `X::TypeCheck::Binding::Parameter` a call
would raise. `p6trialbind` is a port of Rakudo's `Binder.trial_bind` that uses
the multi-dispatcher's type check. Where Rakudo's own test is wrong (it answers
"always binds" for `$a where 1`), the port answers "not sure", the safe
answer.

`p6store`, `p6sink`, `p6return` and `p6invokeflat` compile to the Raku code
Rakudo desugars them into: an assignment, a sink, a `return` and `$code(|@args)`.
A value op would only see a decontainerized operand, and `p6sink` must not
sink a variable's container.

Nine ops are still missing: `p6argvmarray`, `p6bindsig`, `p6trybindsig`,
`p6stateinit`, `p6setfirstflag`, `p6takefirstflag`, `p6setpre`, `p6clearpre`
and `p6staticouter`. They need per-frame state that mutsu does not keep yet,
such as the call's raw capture, the first-run flag of a closure clone, the
`FIRST`/`PRE` flags and the static outer chain.
