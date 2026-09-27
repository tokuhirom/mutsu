# Built-in ancestry has one fewer table, and `.^can` stops over-claiming

ADR-0051 P2 (partial) and P5 landed (#9893).

`Registry::builtin_mro_table`, a hardcoded MRO table that duplicated the
builtin type catalog, is gone: `class_mro` now reads the catalog for every
unregistered builtin. The table disagreed with Rakudo in two places. It gave
`Distribution::Path`/`Hash` a `Distribution` parent and gave
`CompUnit::Repository::Installation` three `CompUnit::Repository*` parents,
but Rakudo composes all of those as roles. The catalog's `roles` now hold
them, and type matching consults them, so `Distribution::Path ~~ Distribution`
still holds. `CompUnit::Repository::FileSystem ~~ CompUnit::Repository::Locally`
also becomes true, as it is in Rakudo.

`.^can` no longer claims methods that Rakudo does not define. Before this
change, `Match.^can("succ")`, `Any.^can("lazy")`, `Cool.^can("base")`,
`Complex.^can("polymod")` and `Pair.^can("lazy")` each answered 1. The cause
was native-method rows filed under the wrong owner. Those rows were removed,
and the types that really have these methods got their own rows:
`Instant`/`Duration` for `succ`/`pred`/`base`/`polymod` through `Real`, and
`Map` for `lazy`. `Instant.succ`/`.pred` and `Duration.succ`/`.pred` had
never been implemented, and now they are.

The hand-maintained 94-name `cool_only_builtin_method` list is retired. The
set is now derived once from the row catalog: names with a `Cool` row but no
`Any`/`Mu` row, plus a short list of names that Rakudo defines only on `Cool`
subtypes. The derived set also contains names the hand list had missed, for
example `printf`, so a plain class's `G.new.printf` now dies as it does in
Rakudo.

The remaining ancestry tables are tracked in #9948: the multi-dispatch
narrowness chains, the `Cool` allowlist, `isa_check` and `is_supertype_of`.
That issue also covers the receiver-blind calls that still answer where
Rakudo dies, such as `$/.succ`.
