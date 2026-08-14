# A method's `callframe(N).file` reported the caller's script, not the method's own module

`todo/tickets/callframe-line-and-file-come-from-different-frames.md` recorded a
real symptom in rakudo's own `Test.rakumod` (run verbatim under
`MUTSU_REAL_TEST=1`): a failing `throws-like` subtest reported
`at t/dotassign-store-and-container-topic.t line 666`, a line number that only
exists inside `Test.rakumod` (953 lines), not in the 106-line test file. The
ticket's working theory was that `push_caller_env` built `.file` (from
`executing_source_file()`, which walks `routine_stack`) and `.line` (from the
VM's global `cur_source_line` tracker) from two frames that had drifted apart
around a bare-block skip.

Re-investigating found that specific symptom no longer reproduces on any of
the four originally-affected files (`t/dotassign-store-and-container-topic.t`,
`t/export-bareword-tag-undeclared.t`, `t/method-private-errors.t`,
`t/obsolete-diamond.t`) — all four now pass cleanly end to end under
`MUTSU_REAL_TEST=1`, so whatever genuine mutsu gap made their assertions fail
in the first place has since been fixed independently, and the location report
was never exercised again.

The underlying bug class described by the ticket's title — `callframe(N).file`
and `callframe(N).line` describing two different actual source positions — was
still real, just in a different mechanism: **method frames**. `RoutineFrame`
carries a `def_file` field that `executing_source_file()` prefers over the
dynamically-scoped `?FILE`, and both `push_routine_with_location` (subs) and
`push_block_routine_with_location` (blocks/closures) populate it from
`FunctionDef::source_file` / `SubData::source_file`. `push_method_routine_with_location`
never did — it hardcoded `def_file: None` unconditionally. So the very first
method frame `executing_source_file()`'s reverse walk encountered (it is never
`is_block`, so the walk never skips past it) broke immediately and fell back to
`current_source_file()` — the env `?FILE`, which module loading only scopes to
a module's *mainline*, and which has therefore already reverted to the calling
script by the time one of that module's methods actually runs. Meanwhile
`.line` is driven by the bytecode line table (`sync_source_line`), which is
unaffected and kept reporting the method's real position — so the pair split
across two files exactly the way the ticket described, just for a method call
rather than a bare block:

```
$ mutsu -I t/lib tmp/probe2-main.raku    # before the fix
level 0: file=tmp/probe2-main.raku line=6     # WRONG file (probe2-main.raku is 3 lines)
```

Fixed by adding `MethodDef::source_file` (populated at method registration
time from `self.current_source_file()`, mirroring the existing `FunctionDef`
sibling fields in the same registration functions; `SubData::source_file` for
a `.^add_method`-installed closure; `None` for synthetic/native methods with
no real declaration site) and threading it into `push_method_routine_with_location`'s
new `def_file` parameter at both bytecode call sites
(`call_compiled_method`/`call_compiled_method_fast` in `vm_method_dispatch.rs`).

```
$ mutsu -I t/lib tmp/probe2-main.raku    # after the fix
level 0: file=tmp/lib/Probe2.rakumod line=6   # file and line now name the same frame
```

Pinned by `t/callframe-file-line-same-frame.t` (with a new `FixtureMethodProbe`
class in `t/lib/FileVarFixture.rakumod`), which calls `callframe(0)` from
inside a module method and asserts `.file` and `.line` both describe the exact
same pinned source line — the shape that used to split across two files. The
original ticket is closed: its concrete repro is gone, and the general
`callframe` file/line-coherence gap it flagged is now fixed for methods too
(subs and blocks were already correct — see
`news/2026-08/module-file-var-and-callframe.md`).
