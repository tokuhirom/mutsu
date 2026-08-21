# The "You planned N test, but ran 0" summary now goes to stdout

When a `Test`-using program declares a plan but never runs a single
assertion, rakudo's `Test.rakumod` prints the closing "You planned N
test(s), but ran M" diagnostic to **stdout**. mutsu printed it to
**stderr** instead, so any program that only inspected stdout (as real
`raku` scripts do) silently lost the line:

```raku
use Test;
plan 1;
say "done";
```

```
$ raku file.raku 2>/dev/null
1..1
done
# You planned 1 test, but ran 0
$ mutsu file.raku 2>/dev/null      # before this fix
1..1
done
```

The divergence was specific to the zero-run edge case. The ordinary "ran
fewer than planned but at least one" mismatch already matched rakudo on
stderr:

```raku
use Test;
plan 2;
ok True, "one";
say "done";
```

```
$ raku file.raku 1>/dev/null
# You planned 2 tests, but ran 1
```

## Fix

`Interpreter::finish()` (`src/runtime/run.rs`) now routes the plan-mismatch
diagnostic to stdout via `emit_output()` specifically when `ran == 0`, and
keeps writing to `stderr_output` for every other mismatch. While in there,
the message's pluralization was also corrected to match rakudo exactly
("test" for a plan of 1, "tests" otherwise) — mutsu previously always said
"test" regardless of the planned count.

Three existing subprocess-shape assertions in
`t/lives-ok-dies-ok-last-next-redo-propagates.t` exercise programs whose
plan is fully unrun (a `last`/`next` escapes past every `lives-ok`/`dies-ok`
before an assertion is ever recorded); their expected `out` strings were
updated to include the now-correctly-routed summary line.

Pinned by the new `t/tap-planned-zero-ran-stream.t`, which spawns both
shapes as subprocesses and asserts the summary lands on the stream real
`raku` puts it on.
