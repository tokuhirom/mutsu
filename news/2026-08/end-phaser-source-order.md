# END phasers run in reverse SOURCE order, mainline and block alike

```raku
{ END { say "END1" } }
{ END { say "END2" } }
END { say "END3" }
```

```
raku : END3  END2  END1
mutsu: END2  END1  END3
```

`todo/tickets/end-phaser-run-order-is-not-reverse-installation.md` insisted the
full ordering contract be measured against `raku` before anyone flipped the
comparator, because `news/2026-08/end-phasers-run-in-install-order.md` had
deliberately made mutsu install-ordered to fix a different bug. It was measured;
the contract is simpler than the three classes mutsu had.

## The contract

Rakudo installs an END as its compiler walks past it, so **a mainline END and
one inside a block are not two classes** — they interleave in one reverse-source
sequence. A module's ENDs are separate only because its compunit is compiled at
the `use`; an `EVAL`'s are separate only because that snippet is compiled at run
time (which is what `File::Temp`'s `03-tempfile.rakutest` turns on).

mutsu had `MODULE < MAIN < RUNTIME`, with `MAIN` covering the main compunit's
top-level ENDs — hoisted and registered eagerly so they still run when the body
dies — and `RUNTIME` covering everything registered from a block, a sub or an
`EVAL`. Reversing that put every block-scoped END ahead of every mainline one.

## The fix

`MAIN` now covers **both**, ordered by the END's **source line** rather than by
when it happened to be registered; `RUNTIME` is narrowed to `EVAL` alone. The
eager hoist tracks the `SetLine` markers as it filters the top-level ENDs out of
the body, and `exec_phaser_end_op` passes `current_source_line()` — withholding
it inside an `EVAL`, whose line numbers mean nothing in the main compunit's
numbering.

## What the measurement also found

Two larger gaps, split off into
`todo/deep/end-phasers-install-at-compile-time.md` rather than forced into this
change: rakudo runs an END inside a block that never executes or a sub that is
never called (mutsu registers on execution, so it does not), and several ENDs on
one *physical line* still tie under line-granularity ordering. Both need the
same thing — a real per-END compile-time source index — so they are one unit of
work.

## Coverage

`t/end-phaser-source-order.t` (5 assertions, passing under `raku` too): a block
END after a mainline one, a mainline END written last still running first, a
mainline/sub/block interleave, a mainline END between two block ENDs, and the
eager hoist that keeps an END running when the body dies before reaching it.
`t/end-phaser-module-order.t` and `roast/S04-phasers/*` stay green.
