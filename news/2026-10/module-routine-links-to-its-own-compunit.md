# A loaded module's routine no longer sees the importing program's lexicals

A routine declared at the top level of a loaded module now links its frame to
its own compunit rather than to the program scope when it names a lexical by
string (ADR-12529 phase 3). Before, a lexical such a routine's frame did not
bind continued at the bottom of the caller chain, which is the importing
program's top-level scope. So a module routine's `EVAL '$x'`, `::('$x')`,
`EVAL 'Foo'` or `::('Foo')` read the program's own `my $x` and the types and
constants the program's top-level `use`s imported. Rakudo reports none of
them.

The frame of such a routine is marked `Env::mark_outer_is_foreign_unit`. A
lexically scoped name or a sigilless term that misses the frame stops there,
and the module's own file scope answers from the per-compunit stores. While a
module's body runs, or a re-`use` re-runs its `sub EXPORT` over the env its
body left, its routines keep the old link, because their file-scope lexicals
are still in that env. Routines that name nothing by string keep the old link
too: a body the interpreter runs over its caller's env (a `Proxy` FETCH) still
reaches the program scope through their frames, which ADR-12529 phase 4
removes.

What had relied on the leak now reads the module's own scope: a module
routine's `EVAL` and `::()` find the module's own types by their short names
(`OwnClass` for `M::OwnClass`) and the module's own package name, and a type an
`EVAL` declares there constrains a variable there.

Found on the way and fixed: a routine's own `my @log` or `my %seen` in a module
whose file also declares `@log` or `%seen` lost element and key stores, or sent
them to the file's container. The name-keyed element store now runs with the
routine's own container bound in its frame, and the compunit's file-scope
variable steps aside for it.
