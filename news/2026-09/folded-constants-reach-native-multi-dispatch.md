# A folded constant expression is now a literal for native multi dispatch

`multi sub d(int $x) {...}` / `multi sub d(Int $x) {...}` ranked `d(5)` on the
native candidate but `d(5 + 0)` on the boxed one. rakudo constant-folds during
optimization, so by the time dispatch runs the two call sites are
indistinguishable and both answer `native`.

## What was missing

The dispatch side was already right: it reads a compile-time mask of "this
position was written as a literal" (`Compiler::literal_native_args_mask`,
`OpCode::CallFunc::literal_native_args`). The mask was computed from the raw
AST, so a live `Binary { op: Plus, .. }` argument left its bit clear even
though the very same expression was about to be folded to `LoadConst 5` a few
lines later in the compiler.

`is_native_literal_arg` now falls back to `Compiler::const_operand` — the
existing constant-folding evaluator from ADR-0006 §2.1 — instead of a private
walker. That is what makes the change sound rather than a guess:

- **Operator overrides.** `const_operand` is only consulted when
  `const_fold_enabled()` says the unit may fold, which is false as soon as a
  `sub infix:<+>` is compiled anywhere in the unit (and false process-wide once
  any user operator is registered at runtime). A unit that overrides `+` falls
  back to the literal-only reading, so `sub infix:<+>($a,$b) { 999 }; d(5 + 0)`
  answers `boxed` — exactly as rakudo does.
- **A declaration that turns up later.** The mask now calls
  `FoldCtx::note_folded()` when it relies on a fold, so an operator declared
  *after* the call site still triggers the unit's refold pass and the mask is
  recomputed with folding off.
- **Native width.** A fold that leaves the i64 range produces a `BigInt`, which
  the mask does not accept, so `d(2**35 * 2**35)` still ranks boxed.
- **The literal shapes are recognised without folding at all**, so `d(5)` and
  `d(-3)` keep ranking native even in a unit where folding is disabled.

Because `const_operand` evaluates with the same native operator
implementations the VM uses, `Int`/`Num`/`Str` folds all follow: `s('a' ~ 'b')`
ranks on `str` and `n(1e0 + 1e0)` on `num`.

Pinned by `t/folded-constant-native-multi-dispatch.t` (12 assertions, all
passing under rakudo as well); `t/const-fold.t`,
`t/const-fold-infix-override.t`, `t/module-exported-infix-no-constant-fold.t`
and `t/native-literal-multi-dispatch.t` are unmoved.
