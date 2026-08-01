# `$?FILE` is a compile-time constant again, and `callframe` reports the file a frame's code lives in

Two related gaps defeated any routine that answers "where was I called from,
outside my own file" — the shape a test-assertion library uses to attribute a
failure to the test script rather than to itself.

`$?FILE` was not folded at all. The parser left it as a plain `Expr::Var("?FILE")`
and the runtime answered it from the env entry of the same name, which module
loading scopes to the module path only *while the module's mainline runs*
(`run_modules.rs`). By the time one of the module's routines was actually called
that scope was long gone, so `$?FILE` inside a module reported the main script.

`callframe(N).file` read the same env entry, so every frame — including frames
executing inside a module — reported whatever file happened to be loading.

Both now answer the question they are supposed to answer:

- **`$?FILE` folds at parse time**, exactly as `$?LINE` and `$?TABSTOP` already
  did, to the file of the compilation unit being parsed. A new parser-side
  `SOURCE_FILE` thread-local carries it; `run()` sets it to the script and
  `parse_module_source` swaps in the module path for the duration of that
  module's parse (a swap, not a set, because module parses nest). Where no file
  is known — `EVAL` and other synthesized parses — the old runtime lookup still
  applies, so nothing regresses there. The fold covers the interpolated spelling
  (`"... $?FILE ..."`) too.
- **`callframe` uses the executing routine's defining file.** The new
  `Interpreter::executing_source_file` prefers the top routine frame's
  `def_file` — the field backtrace rendering already prefers for the same reason
  — and falls back to the env entry. Block frames now carry a `def_file` of
  their own when they come from a closure value (`SubData::source_file`), so a
  block written in the caller's file stays attributed there even while a module
  invokes it; an inlined bare block still records none and is attributed to the
  routine that lexically encloses it.

Pinned by `t/module-file-var-and-callframe.t` (with `t/lib/FileVarFixture.rakumod`),
which passes identically under `raku` — the assertions compare with `.contains`
because rakudo spells `$?FILE` absolutely and appends the module name.

The motivating case is rakudo's own `Test.rakumod`, which locates a failing
assertion by walking frames until one is outside its own file:

```raku
repeat {
    $caller = callframe(++$level);
} while $?FILE.ends-with($caller.file)
     || $caller.file.ends-with($?FILE);
```

With `$?FILE` equal to the *script*, that loop matched the very first frame and
walked off the end of the stack, so every passing assertion worked and the first
failing one died with `No such method 'file' for invocant of type 'Any'`.
Running the genuine upstream module now reports failures exactly as raku does:

```
$ mutsu -I tmp/core tmp/core/tfail.raku
1..3
ok 1 - pass
not ok 2 - deliberate failure
# Failed test 'deliberate failure'
# at tfail.raku line 4
not ok 3 - is failure
# Failed test 'is failure'
# at tfail.raku line 5
# expected: '42'
#      got: '41'
```

See `todo/tickets/vendor-real-test-module.md` for what is left.
