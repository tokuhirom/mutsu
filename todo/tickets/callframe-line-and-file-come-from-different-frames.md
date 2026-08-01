# `callframe(N)` takes its `.file` from the frame and its `.line` from the VM

`push_caller_env` (`src/runtime/runtime_caller_env.rs`) builds each
`CallFrameEntry` from two unrelated sources:

```rust
let file = self.executing_source_file().unwrap_or_default();  // the routine stack
let line = self.cur_source_line;                              // the VM's line tracker
```

`executing_source_file` answers "which file was the currently-executing routine
defined in" by walking the routine stack, while `cur_source_line` is whatever
statement marker the VM last passed. Those two agree for an ordinary call, but
not when the routine stack's top frame belongs to a block whose body is not what
the VM is currently executing — the frame then reports one file and a line
number belonging to a different one.

`$?FILE` was equally wrong before `news/2026-08/module-file-var-and-callframe.md`
(it named the running script rather than the defining file), so the pair was
*consistently* wrong and this mismatch was invisible. It is not a regression from
that change — the rendered output is byte-identical — but it is now the
remaining half of the same problem.

## Repro

Under rakudo's real `Test.rakumod` (aliased as `Test2`, see
`todo/tickets/vendor-real-test-module.md`), a `throws-like` whose inner
`right exception type` subtest fails reports:

```
    not ok 2 - right exception type (X::Assignment::RO)
    # Failed test 'right exception type (X::Assignment::RO)'
    # at t/dotassign-store-and-container-topic.t line 666
```

`t/dotassign-store-and-container-topic.t` is 106 lines long. Line 666 is in
`Test.rakumod` — it is the `ok $type_ok, "right exception type (...)"` call
inside `throws-like`. So `proclaim`'s location walk stopped at a frame whose
`.file` is the test script and whose `.line` came from the module.

Three files in the 1-in-9 sweep sample show it, all through `throws-like`:
`t/dotassign-store-and-container-topic.t`,
`t/export-bareword-tag-undeclared.t`, `t/method-private-errors.t`,
plus `t/obsolete-diamond.t`.

## What a fix has to do

Record the line on the same frame the file comes from. `RoutineFrame` already
carries a call-site `line`/`file` pair *and* a `def_file`; what is missing is the
defining-frame's *current* line, i.e. where inside that routine control sits at
the moment a nested call is made. That is exactly what `cur_source_line` holds
while that routine is the one running, so the natural shape is to save/restore
it per routine frame rather than reading the global at push time.

Beware the second-order effect: `pop_caller_env` restores `cur_source_line` from
the popped entry, so the entry's `line` is load-bearing for the caller's own line
tracking, not only for `callframe`. Splitting the two uses (a `restore_line` and
a `report_line`) is probably cleaner than trying to make one value serve both.
