# A module's file-scope `my` is no longer the caller's variable

A module body runs in the env of whatever frame loaded it — `run_modules.rs` says
so in as many words. Its file-scope `my` declarations therefore landed in that
flat env under their plain key (`$output` → `output`), where a script's own
`my $output` uses the same key. The two were not merely visible to each other:
they were one storage cell, and writes went both ways.

```raku
# ML.rakumod
unit module ML;
my $secret = "module";
sub peek() is export { $secret }
sub poke($v) is export { $secret = $v }
```
```raku
use ML;
my $secret = "script";
say peek();       # raku: module   mutsu was: Nil
poke("poked");
say peek();       # raku: poked    mutsu was: poked
say $secret;      # raku: script   mutsu was: poked   <-- leak
```

Removing the script's `my $secret` made everything correct, so this was purely a
name collision — and note the first line: the module's own initializer was not
even visible to its own routine once the script declared the same name.

## The store

`Interpreter::unit_lexicals` (`module -> name -> shared cell`) is now the home of
a `unit` compunit's file-scope `my` scalars. `load_module` collects those names
from the module's own top-level `VarDecl`s, and once the body has run it moves
each value out of `env` into a `ContainerRef` cell and restores whatever the
loading scope had under that name (removing the key when it had nothing). Cells,
not snapshots: `_init_io` assigning `$output` has to be visible to `proclaim`
reading it.

The module's own routines find it there because they already run with
`current_package` set to the unit package — the named-call path installs
`CompiledFunction::package` on every call — so the lookup is the same shape as
the existing `package_lexicals` one, consulted **before** `env` rather than as a
last resort. Two by-name chokepoints carry it: `get_env_with_main_alias` reads
the cell, `set_env_with_main_alias_sym` writes through it. `set_env_plain_lexical`
deliberately does not redirect, so a routine's own plain `my` shadowing a
compunit lexical stays a distinct variable.

Four details were needed beyond those two chokepoints:

- **Which package owns the running frame is not `current_package`.** A role method
  reached through a mixin — `$fh does File::Temp::AutoUnlink`, which is how
  `File::Temp` arms its temp-file handles — runs with both `current_package` and its
  method class set to `IO::Handle+{File::Temp::AutoUnlink}`, a name no `::` walk
  reduces to `File::Temp`. The routine frame already records `lexical_package` ("the
  package whose compunit lexical routines are visible to this frame"), so that is the
  first candidate, with `lookup_in_running_package`'s three (method class, frame
  package, current package) behind it. Missing this made `File::Temp`'s `DESTROY`
  read a Nil lock, skip its `protect` block entirely, and silently never unlink —
  caught by the bundled-battery gate, not by `t/` or roast.

- An END phaser declared in a `unit module` compiles under the plain package
  name, which auto-qualifies its free variables, so the store also has to answer
  `MP::output` — but only when the qualifier *is* the current package, since an
  explicitly written `$Other::x` is a package variable and must never reach a
  `my` lexical. Same rule `package_scope_lexical` applies.
- `GetUpvalue`'s by-name fallback — the shape a module sub's free read actually
  takes — needed the same redirect as `GetGlobal`.
- A module routine writing its own compunit lexical is *not* a captured-outer
  write, so it must not be replayed into the caller's local slot through
  `free_var_writes` → `pending_rw_writeback_sources`. Without that filter the
  module's write pushed the caller's same-named `my` to whatever `env` happened to
  hold, which after the move was nothing: `my $v = "X"; poke2(); say $v` printed
  the type object.

Pin: `t/module-file-scope-lexical.t` with `t/lib/UnitFileLexical.rakumod`.

## Scope: scalars

`@`/`%` compunit lexicals are deliberately left in `env`. A scalar is read and
written through the two chokepoints above; a container is not — every mutating
method resolves its receiver by name straight out of `self.env`
(`call_method_mut_with_values` plus the ~20 `env_mut().get_mut(name)` sites), so
taking the container out of `env` would make `@a.push` mutate something else
rather than isolate it. That half is
`todo/tickets/module-file-scope-array-and-hash-still-share-the-caller.md`, and it
is cheapest after the `arc_contents_mut` chokepoint consolidation ADR-0001 fuses
with GC.

## Why it mattered now

It was the largest single identified cause in the real-`Test` roast residue
(`todo/tickets/vendor-real-test-module.md`). `Test.rakumod` has nine file-scope
lexicals — `$output`, `$failure_output`, `$todo_output`, `$todo_reason`,
`$subtest_todo_reason`, `$subtest_callable_type`, `$indents`,
`$num_of_tests_planned`, `@vars` — and 13 of the 271 regressing whitelisted files
declare one of those names themselves. Nine aborted mid-file with

```
No such method 'say' for invocant of type 'Str'
  in sub proclaim at .../Test.rakumod line 787
```

because the file's own `my $output = ''` was what `proclaim`'s
`$output.say: $tap` reached — and worse, `_init_io`'s assignment landed on the
*script's* cell, silently replacing its `$output` with an `IO::Handle`. Eight of
those nine names are scalars, so they are covered.

It was never a `Test`-specific problem: any module with a file-scope `my` whose
name a consumer happens to reuse was affected, which is an ordinary
ecosystem-compatibility hazard.
