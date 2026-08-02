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

Pin when fixed: extend `t/module-file-scope-lexical.t` (and
`t/lib/UnitFileLexical.rakumod`) with the array/hash cases that were written and
then removed when the slice was scoped to scalars.
