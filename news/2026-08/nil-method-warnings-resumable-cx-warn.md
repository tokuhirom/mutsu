# `Nil.Real`/`.Int`/`.Str`'s warning is now a catchable `CX::Warn` control exception

`Nil.Real`, `Nil.Int`, `Nil.Str`, and the other Nil-coercion warn-and-resume
methods (`.Num`, `.Rat`, `.FatRat`, `.Complex`, `.Numeric`, `abs`/`floor`/
`ceiling`/`round`/`truncate`/`sign`, `.ords`, `.chrs`) previously raised their
warning as a bare `RuntimeError::warn_signal_with_resume` directly from
`nil_predispatch_error` (`src/vm/vm_call_method_ops.rs`), instead of going
through `Interpreter::raise_resumable_warning` — the shared entry point every
other op-level warn site already used. That entry point's own doc comment
explains why the shortcut is unsafe: the unwinding `CX::Warn` signal it skips
carries its resume value in `return_value`, which an ordinary function-call
boundary treats as an explicit `return` — so a `CONTROL { when CX::Warn {
... } }` handler (the mechanism real `Test::Util`'s `warns-like` relies on to
catch a warning) never saw it. The warning printed straight to stderr instead
of being catchable, so `t/any-type-object-int-coercion.t` and
`t/bound-nil-method-warn.t` failed their `warns-like` assertions the moment
they were switched from mutsu's native `Test`/`Test::Util` fallback to the
real, vendored `Test::Util.rakumod`.

Fixed by splitting `nil_predispatch_error`'s `Option<RuntimeError>` return
into a `NilPredispatchVerdict` enum (`Error` for the hard element-mutator
errors, `Warn { message, resume }` for the coercion warnings) and renaming it
to `nil_predispatch_verdict`. Both call sites (the scalar `MethodCall` opcode
in `vm_call_method_ops.rs` and the named-receiver `CallMethodMut` opcode in
`vm_call_method_mut_ops.rs`) now route the `Warn` case through
`self.raise_resumable_warning(message, resume)?` — which correctly checks for
an active `CONTROL` handler, runs a resume-safe one inline, or falls back to
printing to stderr when nothing is listening — instead of directly
propagating the old bare exception.

With the blocker fixed, completed the migration those two test files had
deferred: both now `use Test::Util;` (the real, vendored module) instead of
relying on mutsu's native fallback, and pass in full.

Full `t/` suite (3183 files) clean on debug and release; verified a targeted
roast sweep of files touching `CX::Warn`/`warns-like`/Nil coercion
(`S02-literals/allomorphic.t`, `S02-literals/quoting.t`,
`S02-types/capture.t`, `S02-types/nil.t`, `S03-operators/repeat.t`,
`S04-exception-handlers/control.t`, `S17-promise/start.t`,
`S17-supply/interval.t`, `S32-basics/warn.t`, `S32-exceptions/misc.t`) all
pass on release.
