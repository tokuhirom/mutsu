# TAP failure diagnostics pick the stream rakudo picks

`Test.rakumod` keeps two output handles, `$todo_output` (stdout) and
`$failure_output` (stderr), and `_diag` chooses between them with

```raku
my $is_todo = !$force-stderr && !$force-informative
    && ($subtest_todo_reason || $num_of_tests_run <= $todo_upto_test_num);
```

— i.e. **by whether the failure is TODO'd**, either in its own right or because
the enclosing subtest is. `subtest` captures the latter when it *starts*
(`my $parent_todo = $todo_reason || $subtest_todo_reason`), so a subtest that
the parent's `todo 1` covers sends everything under it to stdout.

mutsu chose by nesting depth instead (`!effective_todo && subtest_depth() == 0`),
which is a decent approximation of that rule — a diagnostic raised inside a
TODO'd subtest does belong on stdout — but it is wrong for the far more common
case of an ordinary failure inside an ordinary subtest, which rakudo puts on
stderr, indented to its subtest level. It also meant a failing subtest never
emitted its own `# You failed N tests of M`.

Three fixes, all mirroring the module:

- the choice is now `!effective_todo && !subtest_todo_active()`, with
  `subtest_todo_active` a new stack on `TapState` that mirrors
  `$subtest_todo_reason` and is pushed at `begin_subtest` from the parent's
  pending `todo` range (plus the enclosing subtest's own flag, so it is
  inherited all the way down);
- a stderr diagnostic carries its own `"    " x depth` indentation, since it
  bypasses the stdout buffer that `finish_subtest` indents;
- a failing subtest closes with `# You failed N tests of M` on stderr, unless the
  subtest itself was TODO'd.

A fourth, unrelated to the stream choice: `emit_test_failure_diag` both pushed
the message into `stderr_output` *and* `eprint!`ed it, so `flush_stderr_buffer`
printed every failure diagnostic a second time at exit. It now goes through
`emit_stderr`, the existing helper that buffers in nested mode and writes
through otherwise — which also stops the in-process `is_run` from leaking its
child's diagnostics onto the real stderr.

With this in, `roast/S24-testing/12-subtest-todo.t` passes under the *real*
`Test::Util` — its `is_run` predicates count `Failed` occurrences per stream —
and stdout is byte-identical to `raku`'s for the whole file.

Pin: `t/subtest-failure-diag-stream.t`, verified under both implementations.
