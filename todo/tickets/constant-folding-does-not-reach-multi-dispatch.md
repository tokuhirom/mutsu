# A folded constant expression is not a literal for multi dispatch

Found 2026-09-07 while fixing
`news/2026-09/native-literal-multi-dispatch.md`, where it was the one row of a
24-row measurement matrix that still diverged. Independent of that change: it is
a constant-folding question, not a dispatch one.

## Repro

```raku
multi sub d(int $x) { "native" }
multi sub d(Int $x) { "boxed" }
say d(5);       # raku: native   mutsu: native  -- correct
say d(5 + 0);   # raku: native   mutsu: boxed
```

rakudo constant-folds `5 + 0` back to a literal at compile time, so the call
site is indistinguishable from `d(5)` by the time dispatch runs. mutsu does not
fold it: `--dump-ast -e 'd(5 + 0)'` shows a live `Binary { op: Plus, .. }`
argument (`literal_value: None`), so the call site's `literal_native_args` mask
leaves that position clear and the argument ranks as an ordinary runtime `Int`.

## Why it is a ticket

The dispatch side is already right and needs no change: it reads a compile-time
mask of "this position was written as a literal"
(`Compiler::literal_native_args_mask`, `OpCode::CallFunc::literal_native_args`).
The gap is upstream — mutsu folds fewer constant expressions than rakudo, and
this is only the first place the difference became *observable*. Fixing it means
deciding how much constant folding mutsu should do and where, which is a
compiler design question with its own blast radius (`BEGIN`-time semantics,
overflow, `Rat` promotion, operator overloading of `infix:<+>` on `Int`) rather
than a one-line dispatch tweak.

Note that operator overloading is the trap here: a user may redefine
`infix:<+>` for `Int`, so folding `5 + 0` unconditionally at parse time is not
sound. rakudo folds during optimization, after it knows the operator resolved to
the core candidate.

## Where to look

`Expr::Literal`'s `literal_value` slot in `src/ast.rs` (already there and
already `None` for this shape), whatever populates it, and
`Compiler::is_native_literal_arg` (`src/compiler/mod.rs`), which is the consumer
that would then see the folded value for free.

## Check when fixing

`d(5 + 0)` answers `native`; `d(5 + $x)` for a runtime `$x` still answers
`boxed`; a folded expression that overflows the native width
(`d(2**35 * 2**35)`) still answers `boxed`; a user-defined `multi
infix:<+>(Int, Int)` is NOT folded away; and
`t/native-literal-multi-dispatch.t` still passes.
