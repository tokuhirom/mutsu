# An exported sub in a package block reads the block's lexicals after import

An `is export` sub declared in a block of an inline module closes over the
block's `my` variables:

```raku
module M { if True { my $x = 5; sub g is export { $x } } }; import M; say g()
```

Rakudo prints `5`; mutsu printed `Nil` (#10559). `import M` runs at BEGIN time,
before the block runs, so the importer aliased the routine the CHECK-time
prepass had registered. A named sub reads its free variables by name, and by
the time the import alias was called the block's scope was gone.

A sub declared in a block of a non-`GLOBAL` package body now binds its free
variables per activation of that block. This is the mechanism a sub nested in
a routine already uses (mutsu#9111, Rakudo's `capturelex`). Each run of the
declaration binds hidden aliases to the variables' cells and records them as
the routine's latest-activation cells. The cells are keyed by the routine's
identity (name, package, defining file). The import alias, the `EXPORT`
stash entries and the in-sequence registration are all that one routine, so
every call reads the latest activation's lexicals. A mainline block of
`GLOBAL` keeps ADR-0024's per-block bucket.

No name-keyed side table grows: the change only widens where the existing
per-activation binding applies.
