# Identity::Utils selective imports: lexical multis, `&` shadowing, `:p` exports

A random draw from the ecosystem ledger (Identity::Utils 0.0.29, locked on
#8977) turned up four general interpreter bugs behind one failing file,
`t/02-selective-importing.rakutest`. It imports each routine in its own bare
block through a `sub EXPORT` that looks names up in `UNIT::`.

- **A module's lexical `multi` family vanished after the block that first
  loaded it.** A package-less module registers `my multi` candidates on
  `GLOBAL::name/<sig>` keys. A single `my sub` is moved into the per-compunit
  private table, but these candidates are not. On block exit, the rollback
  treated every `GLOBAL::` key as a possible import alias and dropped it. The
  module's own subs, and its next `EXPORT` run, then lost the family. Now a
  module-registered `GLOBAL::` routine that no module exports is put back like
  any other module-owned routine. It cannot be an import alias. The helpers
  moved to `src/runtime/module_reinstate.rs`.
- **`&name` reads ignored lexical shadowing.** `GetCodeVar` found a `&f` local
  by probing slot names at run time, and that always returned the first slot.
  So after `{ my &f = &lc }` or a nested `if ... -> &f`, the inner read got the
  outer binding. When the compiler knows the slot, it now emits
  `GetCodeVarLocal { name_idx, slot }`, the same way scalar reads use the
  scoped `local_map`.
- **A pointy `if` parameter leaked into the enclosing scope.** The
  `if EXPR -> $c` desugar declared `my $c` in the surrounding scope. That
  reused an outer `$c`'s slot, so `my $c = 7; if 8 -> $c { }; say $c` printed
  8. The binding now gets its own local scope, in both the statement and the
  value lowering.
- **An EXPORT map of `UNIT::{"&x"}:p` pairs exported uncallable routines.**
  The values of those pairs live in the stash element's container. The bare
  call then tried to invoke the Scalar and died with `No such method
  'CALL-ME'`. Both the String::Utils idiom and `(%h<f>:p).value.(...)` now
  work: an exported `&name` binds the decontainerized routine, and invoking a
  value decontainerizes it first.

Separately, `"&f(ARGS)"` interpolation now takes a trailing method-call chain
(`"&short-name($id).subst('::','-',:g)"`), like an interpolated variable
does. Its arguments are also split at top-level commas only.

With these fixes, Identity::Utils' only actionable baseline file,
`t/02-selective-importing.rakutest`, goes from 37/40 to 40/40. `t/03` stays
at parity. `t/01` has no rakudo baseline, and it now runs all 181 tests
instead of dying at test 60.
