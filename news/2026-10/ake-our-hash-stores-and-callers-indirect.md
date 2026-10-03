# ake: module `our` hash stores, stash hashes and `CALLERS::('$_')`

The `ake` task runner now matches rakudo on every one of its test files.
Before, only `t/10-dispatch` passed; the other files died early. Three
separate bugs were behind this:

- **A module routine's element store into its package's `our %h`.**
  Statements like `%TASKS{$name} = ...` and `%TASKS{$name}++` used to commit
  into the env binding of the loading scope. Every read resolves the
  package's own container, so the key vanished. Both fast lanes now decline
  for an `our` package container, and the increment's write-back goes through
  the package mirror. That mirror is where the read side and the generic
  write chokepoint already resolve the name.
- **`%( Foo::EXPORT::DEFAULT:: )` and `.hash` on a stash.** For a single
  stash these died with "No such method 'hash'". A list of stashes did not
  merge their symbols either. Stashes now flatten the same way `.Hash`
  already flattened them; `sub EXPORT` re-exports use this shape.
- **`CALLERS::('$_')`.** The indirect spelling of `CALLERS::<$_>` died with
  "No such symbol". It now walks the call stack to the caller's variable, as
  `$::('CALLERS::_')` does; ake's test helper uses it to default its working
  directory.
