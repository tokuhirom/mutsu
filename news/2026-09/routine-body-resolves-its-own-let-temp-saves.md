# A routine body resolves its own `let`/`temp` saves

`let` and `temp` resolve their saves at the end of the enclosing **block**, and a routine body
is one: `temp` restores the pre-scope value unconditionally, `let` restores it only when the
block exited unsuccessfully. mutsu implemented that rule for exactly two lowerings — the
statement-position bare block and, since GH-7635, the genuine value-position `do { ... }` —
both of which emit `OpCode::LetBlock`. A routine body is compiled by neither: it goes through
the closure/sub-body path, which emits no `LetBlock` at all. So a save recorded by
`OpCode::LetSave` inside a routine was never resolved by the frame that owned it; it survived
until some outer block happened to resolve it, or forever.

```raku
my $x = 1;
sub f() { let $x = 2; Nil }
f();
say $x;             # raku: 1   mutsu (before): 2
```

Methods diverged for `temp` as well as `let`, because the method-dispatch path discarded the
saves outright on an explicit `return` and did not touch them at all on a natural fall-through:

```raku
my $x = 1;
class C { method m() { temp $x = 2; 42 } }
C.m;
say $x;             # raku: 1   mutsu (before): 2
```

## Resolving at frame teardown

The saves are resolved where the frame is torn down rather than by wrapping a bytecode range,
because a routine has several ways to produce its result — a tail expression, an explicit
`return`, `fail`, an exception unwinding the frame — and `OpCode::LetBlock` can read the value
from only one place. Every dispatch path already took a `let_saves` mark on entry and already
called `resolve_let_saves_on_success(mark, true)` on exit; the hard-coded `true` is what made
`temp` work (it restores either way) and `let` never restore. The new
`Interpreter::resolve_frame_let_saves` takes the frame's actual result value instead, and every
routine, method and closure dispatch path now hands it the value it is about to return —
including the explicit-`return` arms, which previously discarded the saves.

The `try`/implicit-CATCH region got the same treatment. It is a block in its own right, and it
is also how a routine body carrying a `CATCH` phaser is compiled, so
`sub f() { temp $x = 2; CATCH { default { } }; 7 }` had no other place to resolve its saves at
all.

## A tail `let` is an assignment

Judging the exit by the frame's value exposed a second gap: `let $x = 42` is an assignment and
its value is 42, but in block-final position mutsu compiled it through the valueless default
arm and yielded `Nil`. That was invisible while the saves leaked; once the value decides
whether the save is kept, a `sub f() { let $x = 2; }` looked like a failed block and rolled
itself back. The tail-statement lowerings — the routine-body, closure-body, phaser-body and
inline-block ones, alongside the existing expression-position arm — now share
`Compiler::compile_let_stmt_as_value`, which runs the save plus the assignment and pushes the
temporized variable. `do { let $x = 5 }` is `5` now, as it is in rakudo.

## Still open

A loop body is a block too, and it resolves nothing: `for 1..2 { temp $k = 2; }` leaks the
temporized value, and the second iteration observes the first one's. That is filed separately
as GH-7677 — a loop body is compiled in sink context, so the "did this block succeed" value
that `let` needs does not exist for it without a second body-lowering shape.

Pinned by `t/routine-body-let-resolution.t` (28 assertions, each measured against `raku`
first).
