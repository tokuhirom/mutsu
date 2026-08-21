# TAP "You planned N test, but ran 0" summary goes to the wrong stream when zero tests ran

Discovered while validating
`t/lives-ok-dies-ok-last-next-redo-propagates.t` (see
`news/2026-08/lives-ok-dies-ok-last-next-redo-propagation.md`) against real
`raku`. Unrelated to that fix -- pinned here separately.

## Repro

```
$ cat tmp/plan_no_tests.raku
use Test;
plan 1;
say "done";
```

```
$ raku tmp/plan_no_tests.raku 2>/dev/null
1..1
done
# You planned 1 test, but ran 0
$ raku tmp/plan_no_tests.raku 1>/dev/null
(nothing)

$ mutsu tmp/plan_no_tests.raku 2>/dev/null
1..1
done
$ mutsu tmp/plan_no_tests.raku 1>/dev/null
# You planned 1 test, but ran 0
```

Both exit 255 (or 1, depending on build). `raku` prints the "planned N,
but ran 0" summary to **stdout**; mutsu prints it to **stderr**.

## Why this is narrower than it looks

The ordinary "ran fewer than planned but at least one" case already
matches raku on stderr:

```
$ cat tmp/plan_mismatch.raku
use Test;
plan 2;
ok True, "one";
say "done";
```

Both `raku` and mutsu print `# You planned 2 tests, but ran 1` to
**stderr** for this one. So the divergence is specific to the "ran
*zero*" edge case (`tests_run == 0`), not the general planned/run
mismatch summary -- rakudo's `Test.rakumod` apparently special-cases the
zero-run path to a different (stdout) sink, or the whole END-phaser exit
path differs when no test ever fired (there is no unwound-loop
involvement -- reproduces with a plain `plan 1; say "done";` and no
`last`/`next`/`redo`/`lives-ok` anywhere).

## Where to look

- `src/runtime/test_functions.rs` (or wherever the TAP end-of-run summary
  ("You planned N test(s), but ran M") is emitted) -- find the "ran == 0"
  branch specifically and check which output sink it writes to.
- Compare against the emission for the "ran > 0 but < planned" case,
  which is already correct, to see what differs in rakudo's own
  `Test.rakumod` (vendored or upstream) for the zero-run branch.

## Priority

Small, self-contained -- likely a one-line stream fix once the exact
"ran 0" branch is found. Filed as a ticket rather than fixed inline here
to keep the last/next/redo propagation fix's PR focused.
