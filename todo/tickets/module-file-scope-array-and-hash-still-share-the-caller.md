# A module's file-scope `my @a` / `my %h` is still the caller's variable

`news/2026-08/module-file-scope-lexical-is-not-the-callers.md` fixed this for
**scalars**: a `unit` compunit's file-scope `my $x` now lives in a shared cell in
`Interpreter::unit_lexicals`, keyed by the unit package, and the module's own
routines read and write it there instead of under the plain `env` key the loading
scope uses. `@`/`%` file-scope lexicals were deliberately left out of that store,
so for them the original bug stands:

```raku
# UFL.rakumod
unit module UFL;
my @items = <a b>;
sub peek-items() is export { @items.join(",") }
sub push-item($v) is export { @items.push($v) }
```
```raku
use UFL;
my @items = <x y z>;
push-item("c");
say peek-items();        # raku: a,b,c   mutsu: a,b
say @items.join(",");    # raku: x,y,z   mutsu: x,y,z,c
```

## Why the scalar fix does not extend to them

A scalar is read and written through two by-name chokepoints
(`get_env_with_main_alias` / `set_env_with_main_alias_sym`), so one redirect in
each covers every path. A *container* is not: a mutating method resolves its
receiver by name straight out of `self.env`, in
`call_method_mut_with_values` (`src/runtime/methods_mut_dispatch.rs`, which reads
and re-inserts `env[target_var]` in dozens of places) and in the ~20
`env_mut().get_mut(name)` sites across `src/vm/vm_var_*.rs`. Taking `@items` out
of `env` therefore does not isolate the module's array — it makes `@items.push`
mutate whatever *else* is under that key, or autovivify a third array. That is
strictly worse than the shared-storage bug, so the store is scalar-only and
`collect_unit_lexical_names` skips `@`/`%` on purpose.

## What it would take

The mutating-method receiver has to become a single resolution point that the
compunit store can hook, the way the scalar reads/writes already do. That is the
same `arc_contents_mut` / element-cell chokepoint consolidation ADR-0001 fuses
with GC ("layer 3a"), so this is most cheaply done *after* that campaign rather
than by threading a special case through `call_method_mut_with_values` now.

Note the exposure is narrower than the scalar case was: it needs a module with a
file-scope `my @a`/`my %h` **and** a consumer that declares the same name.
`Test.rakumod`'s `@vars` is the one that matters for
`todo/tickets/vendor-real-test-module.md`; the other eight of its file-scope
lexicals are scalars and are fixed.

## A measured instance: `roast/integration/99problems-41-to-50.t` (2026-08-05)

That predicted collision is real and it costs a whole roast file. Under
`MUTSU_REAL_TEST=1` the file aborts after 1 of its 9 assertions with
`unknown variable: A`, raised from the test's own grammar action:

```raku
method truth-table($expr, $actions) {
    my @vars = @( $/.ast<vars> );          # <-- same name as Test.rakumod's
    sub the-truth(@vals) {
        our %*VAR = @vars Z=> @vals;       # built from the WRONG @vars
        ...
    }
}
```

`%*VAR` therefore comes out empty and the `term:sym<var>` closure's
`%*VAR{$id} // die "unknown variable: $id"` fires. Renaming the *test's*
`@vars` to `@varz` makes the file pass, which is the confirmation — the file is
otherwise unmodified and `raku` passes it as written.

Bisecting `Test.rakumod` converges on `sub _push_vars` (`@vars.push: item [...]`),
i.e. the declaration alone is not enough; it takes a routine that mutates the
array by name. Two notes on doing that bisect, since the obvious method does not
work:

- **Do not truncate `Test.rakumod` at a line number.** File scope calls
  `_init_vars()` at line 41 and that routine is declared at line 867, so every
  prefix cut either fails to parse or dies on `Unknown function: _init_vars`
  long before it changes behaviour.
- Split the file into brace-balanced top-level chunks (248 of them) and always
  keep chunks 0-47 plus the `_init_vars` chunk (220). Then a keep-range or
  drop-range bisect converges in about six runs.

So the file is blocked on this ticket, not on anything in `Test`.

Pin when fixed: extend `t/module-file-scope-lexical.t` (and
`t/lib/UnitFileLexical.rakumod`) with the array/hash cases that were written and
then removed when the slice was scoped to scalars.
