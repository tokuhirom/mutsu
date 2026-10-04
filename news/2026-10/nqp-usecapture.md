# `nqp::usecapture` and the capture readers

The capture ops of #11496 (part of the `nqp::` coverage campaign #11488)
are implemented: `usecapture`, `savecapture`, `captureposelems`,
`captureposarg` (and its `_i`/`_n`/`_s` forms), `captureposprimspec`,
`capturehasnameds`, `captureexistsnamed` and `capturenamedshash`.

MoarVM keeps every frame's incoming arguments; mutsu binds arguments
straight into the callee's variables and kept no such record, which is
what made `usecapture` more than a ticket. The record is now made only for
code that asks for it: the compiler flags a code object that reads
`usecapture`/`savecapture` (`CompiledCode::uses_capture`), the frameless
fast and light call paths decline such code, and the named-sub, closure and
method binders store the call's raw arguments as a `Capture` on its VM call
frame. The capture has the call-site shape — an omitted optional is absent,
a slurpy has gathered nothing, a slip is already flattened, a method's
invocant comes first — and code that does not read it pays nothing. It
lives on the frame rather than in the env because a return merge copies a
callee's env writes back into its caller, which would hand a caller its
callee's capture.

Since mutsu passes every argument as an object, `captureposprimspec`
answers 0 and the native `captureposarg_i`/`_n`/`_s` readers raise MoarVM's
error for an object argument.

While here: a parenthesized zero-argument `nqp::` call (`nqp::cwd()`) now
compiles to the `NqpOp` opcode like any other op. The parser marks every
`f()` with a synthetic call-site pair, which kept all of them on the
generic call path.
