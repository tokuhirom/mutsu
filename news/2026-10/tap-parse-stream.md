# TAP's own test suite passes

The `TAP` distribution (0.3.15) parses TAP text through a `supply` block that
splits its input with `.lines`, keeps a lexical `enum Mode <Normal SubTest
Yaml>`, emits through a nested `sub`, and flushes in a `LEAVE` phaser. Six
separate gaps stood between that block and a parsed plan; with them closed both
`t/source-file.rakutest` and `t/string.rakutest` pass:

- `.lines` on an on-demand supply (a `supply { }` block) splits the emitted
  chunks when tapped, and an `emit` from a sub called inside a chained
  `whenever` reaches the right supply.
- Leaving a block that wraps a closure body with phasers no longer resets a
  lexical a hoisted `sub` had captured: the slot's shared cell was synced back
  as Nil, so the `whenever` callback read `Nil`.
- Enum keys declared in a `supply` block resolve to that block's enum inside
  its `whenever` callbacks, even when the enclosing package declares another
  enum with the same key, and keys declared in different routine invocations
  no longer poison each other.
- `(cond ?? $a !! $b)++` (and `--`, prefix forms) increment the selected
  container.
- A `token` with a defaulted parameter, referenced without arguments
  (`<sub-test>`), binds the default.
- Inside a module, its own `class Test` is what the bare `Test` means, also
  when the program loaded the `Test` module.
- `has Str:D @.errors` is an `Array[Str:D]` (and `%` attributes likewise),
  not an `Array[Str]`.
