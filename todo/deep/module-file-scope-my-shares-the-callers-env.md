# A module's file-scope `my` variable is the *same* variable as a same-named `my` in the script

A module body runs in the env of whatever frame loaded it — `run_modules.rs`
says so in as many words ("the module body runs in the CALLER's env"). Its
file-scope `my` declarations therefore land in that flat env under their plain
key (`$output` → `output`), where a script's own `my $output` uses the same key.
The two are not merely visible to each other: they are one storage cell, and
writes go both ways.

## Minimal repro

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
say "1 module: ", peek();     # raku: module   mutsu: Nil
say "2 script: $secret";      # raku: script   mutsu: script
poke("poked");
say "3 module: ", peek();     # raku: poked    mutsu: poked
say "4 script: $secret";      # raku: script   mutsu: poked   <-- leak
```

Remove the script's `my $secret` and everything is correct, so this is purely a
name collision. Note line 1 as well: the module's own initializer is not even
visible to its own routine once the script declares the same name.

## Why it matters now

It is the largest single identified cause in the real-`Test` roast residue
(`todo/tickets/vendor-real-test-module.md`). `Test.rakumod` has nine file-scope
lexicals — `$output`, `$failure_output`, `$todo_output`, `$todo_reason`,
`$subtest_todo_reason`, `$subtest_callable_type`, `$indents`,
`$num_of_tests_planned`, `@vars` — and 13 of the 271 regressing whitelisted
files declare one of those names themselves. Nine of them abort mid-file with
the same signature:

```
No such method 'say' for invocant of type 'Str'
  in sub proclaim at .../Test.rakumod line 787
```

because the file's own `my $output = ''` is what `proclaim`'s `$output.say: $tap`
reaches. Worse, the module writes back: a file that does `my $output = ''` and
then calls `is` finds its `$output` silently replaced by an `IO::Handle`
(`_init_io`'s assignment landing on the script's cell).

It is not a `Test`-specific problem — any module with a file-scope `my` whose
name a consumer happens to reuse is affected, which is an ordinary
ecosystem-compatibility hazard.

## Why it is large

`module_scope_lexicals` (a `HashMap<module, HashMap<name, Value>>`) already
records these names per module, but only as a **last-resort read fallback**,
consulted in `exec_get_global`-family paths *after* every live-env route — its
stated purpose is keeping the binding reachable once the loading frame is gone
(a `require` inside a method), not isolating it. Making it authoritative means:

- **Read side**: a routine whose declaring package owns `name` must resolve to
  the module table *before* the caller's env, not after. The relevant sites are
  `src/vm/vm_var_get_ops.rs` (`module_scope_lexical` fallback, ~line 399),
  `src/vm/vm_exec_dispatch.rs` (three comments referencing the table, lines
  ~433/585/684) and `src/runtime/types/type_registry.rs`
  (`lookup_in_running_package` / `lookup_in_package_chain`).
- **Write side**: nothing routes a *write* there today. `poke("poked")` above
  updates env; with the names removed from env it would have to update the
  module table, so every `SetGlobal`/assignment path needs the same ownership
  check as the read path.
- **Removal from env**: `collect_module_scope_names` copies the names out after
  the body runs but leaves them in env. Removing them is what actually ends the
  collision — and it will expose every read/write path the two bullets above
  missed, all at once, across every bundled battery and vendored dist.
- **Hot path**: these are the variable-access opcodes. An ownership check per
  access has to be gated so a program that loads no module pays nothing (the
  existing fallback is already guarded by `module_scope_lexicals.is_empty()`).

## Where to start

Reproduce with the file above (`tmp/modlex/`), then make the module table
authoritative for the module's *own* routines only, keeping the caller's env
authoritative for everything else — the ownership key is the routine's declaring
package, which `enter_routine_package` already establishes. Land the read side
with the names still duplicated in env (no behaviour change for non-colliding
programs), then remove them from env and fix the write side. `make roast` is the
real review: every bundled battery and every vendored dist goes through this
path.
