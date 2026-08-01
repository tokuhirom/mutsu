# A bare call statement sinks its value, so an unhandled `Failure` throws

A bare call statement discards its value, and in Raku that value is *sunk* —
sinking an unhandled `Failure` throws. mutsu applied the rule only to the call
shapes whose result lands on the VM stack, where `OpCode::SinkPop` performs the
check. A **bareword statement-level call** compiles to `OpCode::ExecCall`, which
leaves nothing on the stack at all, so it never reached `SinkPop` and swallowed
the Failure:

```
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; EVAL q{use fatal; "foo"[2]}; say "after"'
after                                   # raku: Index out of range. Is: 2, ...
```

`ExecCall` / `ExecCallPairs { keep_value: false }` now run the same two checks
`SinkPop` does on the value they are about to discard: an unhandled `Failure`
throws unconditionally, and under `use fatal` so does a reified list or `Seq`
holding one. The check runs *after* the `is rw` / carrier writeback, so a call
that both wrote through to a caller lexical and returned a Failure still lands
its writeback before unwinding.

## How it was found, and what the symptom looked like

Through the Test-vendoring sweep (`todo/tickets/vendor-real-test-module.md`).
rakudo's `Test.rakumod` writes the string form of `throws-like` as a bare
statement:

```raku
EVAL $code, context => $caller-context;
```

so `throws-like 'use fatal; "foo"[2]', X::OutOfRange` reported *code did not
die* — while the block form of the same assertion passed. That made it look like
a `use fatal` scoping bug, and the first narrowing attempts chased the wrong
ingredient (a `CATCH`-bearing block, then `subtest`, then EVAL's `context`
argument). Each of those shapes throws correctly on its own. The tells that
located it were:

- moving the failing expression off the end of the EVAL'd unit
  (`EVAL q{use fatal; "foo"[2]; 99}`) made it throw, and
- *binding* the EVAL instead of sinking it (`my $x := EVAL q{...}`) made it
  throw.

Both point at the discarded value, not at the pragma. `--dump-bytecode` then
showed it directly: a statement-level `f()` emits `CallFuncNamed` + `SinkPop`,
while `EVAL q{1};` emits a lone `ExecCall` with no sink at all.

Two `t/` files regressed under the real `Test` module on this alone
(`out-of-range-scalar-index.t`, `numeric-real-target.t`).

Pinned by `t/statement-call-sinks-its-value.t`, green under `raku` too.
