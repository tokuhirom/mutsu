# A stash held in a variable writes through to its package; `Stash.package_at_key`

`my $WHO = Pkg.WHO; $WHO<k> := v` (and `=`, `.{k}`, `ASSIGN-KEY`, or the same inside a sub taking
the stash as `$WHO is copy`) now installs `k` in the package, as `Pkg.WHO.BIND-KEY(k, v)` always did:
a stash is the package's live symbol table. The write goes to the package's own store even when the
sub belongs to another compunit than a `my package` of an `EVAL`. `Stash.package_at_key($key)` mints
the named package (an empty `Metamodel::PackageHOW` type named after its stash) and binds it, so
CompUnit::Util's `set-in-WHO(tmp.WHO, 'Foo::Bar', ...)` works; a term of a nested package no longer
appears as a `Inner::x` key of its parent's stash. CompUnit::Util `t/01-utils.rakutest` goes from
12 to 15 of 17 passing; the rest (`$*R`/`$*W` resolver, uppercase `our $Baz` stash keys, custom
`my`-scoped package declarators) is filed separately.
