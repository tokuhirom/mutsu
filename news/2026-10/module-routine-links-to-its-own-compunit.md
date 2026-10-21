# A loaded module's routine no longer sees the importing program's lexicals

A routine declared at the top level of a loaded module now links its frame to
its own compunit rather than to the program scope (ADR-12529 phase 3). Before,
a lexical a module routine's frame did not bind continued at the bottom of the
caller chain, which is the importing program's top-level scope. So a module
routine's `EVAL '$x'`, `::('$x')`, `EVAL 'Foo'` or `::('Foo')` read the
program's own `my $x` and the types and constants the program's top-level
`use`s imported. Rakudo reports none of them.

The frame of such a routine is marked `Env::mark_outer_is_foreign_unit`. A
lexically scoped name or a sigilless term that misses the frame stops there,
and the module's own file scope answers from the per-compunit stores. While a
module's body runs, or a re-`use` re-runs its `sub EXPORT` over the env its
body left, its routines keep the old link, because their file-scope lexicals
are still in that env.

Several places had relied on the leak and now read the module's own scope:

- A module routine's `EVAL` and `::()` find the module's own types by their
  short names (`OwnClass` for `M::OwnClass`) and the module's own package
  name. A type an `EVAL` declares there constrains a variable there.
- A regex literal captures a top-level kebab-case variable (`$module-name`),
  as it already captured a short one. `like $s, / ... $module-name /` from a
  test file used to resolve the name inside `Test`'s `like`.
- A regex in a module's class method captures the file's kebab-case
  `constant $bare-pat` from the module's scope table.

Found on the way and fixed: a routine's own `my @log` or `my %seen` in a module
whose file also declares `@log` or `%seen` lost element and key stores, or sent
them to the file's container. The name-keyed stores now skip the compunit's
file-scope variable when the running routine declares the name itself.
