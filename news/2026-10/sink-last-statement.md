# A program's last statement is sunk

Raku sinks every statement of a program, the last one included. mutsu
compiled the mainline's final statement as the unit's value instead, so
nothing in it was sunk unless it happened to be one of the few method-call
shapes a post-run check looked at:

```raku
sub foo { fail "boom" }
foo()                        # raku: dies with "boom"; mutsu: exit 0
```

`shell "exit 1"` as the only statement now dies, like it does mid-program,
and a fresh object as the final statement has its `sink` method called. The
mainline compiler now treats a final expression or call exactly like any
other statement (`Compiler::unit_tail_sinks`), `SinkPop` included (#9766).
An `EVAL`'s final statement, and a REPL line's, are still their value:
the REPL calls the new `Interpreter::run_value_tail`.

Still open: a user `sink` method on a value a *sub call* returns
(`sub g { S.new }; g;`) is not run. That needs the call result to say
whether it is a container, and is tracked in #11537.
