# A user `multi` named like a core routine joins it instead of replacing it

A proto-less `multi sub die(Cool:D $ where .ends-with("\n"))`, the one the `Die`
distribution exports, did nothing for `die "foo\n"`. The `die`/`fail` statement
parser built a `Die`/`Fail` node without checking whether a user or imported sub
shadowed the name. `say`/`print`/`put`/`note` already had that check, so the
statement form now bails out the same way and parses as an ordinary call.

That exposed a second gap. When no user candidate matched (`die "plain"`), the VM
went to `call_function_fallback`, which only consults the native function table.
The interpreter-level builtins (`die`, `fail`, `note`, `say`, ...) are not in
that table, so the call raised `X::Multi::NoMatch` where Rakudo just runs the
core routine. In Raku such a multi adds a candidate to CORE's proto. Now, when
no user candidate matches and the miss is not an ambiguity, the VM dispatches a
core routine name through `call_function`, whose builtin arms cover every core
name. `fail` joined `BUILTIN_FUNCTION_NAMES` so it takes the same path.

Last, `-MModule` loaded the module at run time but never told the parser, so
the mainline parse could not see the module's exports. The `-M` list is now
passed to the parser for the mainline parse only, which matches Rakudo's
reading of `-MFoo` as a `use Foo;`.

`Die`'s `t/01-die.t` now passes 4/4 (was 3/4), so all three of its baseline
files pass.
