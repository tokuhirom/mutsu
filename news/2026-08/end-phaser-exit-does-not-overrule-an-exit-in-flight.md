# An END phaser's `exit` no longer overrules the exit already in flight

Rakudo latches the process status at the *first* `exit`. An `exit` raised while
one is already unwinding — an END phaser's own `exit` in a program that already
said `exit 42` — still ends the block it runs in, but it neither overwrites the
status nor stops the END phasers that have not run yet.

mutsu did the opposite on both counts. `exit` unconditionally wrote
`self.exit_code`, and an END phaser that called it left `halted` set, which the
END runner read as "stop", so every phaser that had not run yet was skipped:

```raku
END { say "A" }
END { say "B"; exit 7 }
exit 42;
```

| | raku | mutsu (before) |
| --- | --- | --- |
| output | `B` `A` | `B` |
| status | 42 | 7 |

The fix is rakudo's `the-end-is-nigh` latch, as an `exit_status_locked` flag on
the interpreter. `finish` sets it when the END phasers are entered because the
program is already exiting, and sets it again after any phaser that calls
`exit`; `builtin_exit` keeps the current `exit_code` instead of the requested one
while it is held. Separately, the END runner now clears `halted` after each
phaser, so an `exit` inside one ends *that* phaser without cancelling its
siblings — which is what rakudo does whether or not the status changes hands.

## Why it surfaced

`todo/tickets/vendor-real-test-module.md`. The real `Test.rakumod` ends with

```raku
exit($num_of_tests_failed min 254) if $num_of_tests_failed > 0;
```

in its END block, so under `MUTSU_REAL_TEST=1` *every* explicit `exit` in a test
file with a failing assertion came back as 1. That is exactly what
`Test::Util`'s `is_run ..., :255status` reads, so `t/die-on-fail.t` — which
asserts rakudo's documented `RAKU_TEST_DIE_ON_FAIL` behaviour, `exit 255` from
`Test`'s own `die-on-fail` — failed under the real module while passing under
mutsu's native provider.

The bug itself has nothing to do with `Test`: the repro above uses no modules at
all. Pinned by `t/end-phaser-exit-latch.t`, which passes under `raku` too.
