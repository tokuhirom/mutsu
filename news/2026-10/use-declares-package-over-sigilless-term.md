# `use X` declares the package `X` over an outer sigilless `\X`

`my \Dep = 1; { use Dep; Dep }` and `"use Dep; Dep".EVAL` read the outer `\Dep`
instead of the package the `use` had just imported: the bareword compiled to a
by-name read of the term key, so a sigilless binding of any enclosing scope won
over the unit's own declaration. The self-referential form
`my \DBIish = try "use DBIish; DBIish".EVAL` therefore gave `Any` (its own
unassigned term), which is how Humming-Bird's `t/opt/plugin-dbiish.rakutest`
ran 5 of 9 under mutsu. A `use X` now records `X` as declared in its scope
(`Compiler::use_declared_names`, restored on block exit, dropped again by a
later `my \X`), where a sigilless binding of that name is visible or the unit is
an `EVAL`'d one, and a bareword `X` after it reads the package. Pinned by
`t/modules/import-export/use-declares-package-over-sigilless-term.t`; the
Humming-Bird file now passes 9 of 9 next to rakudo's skip.
