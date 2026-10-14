# A `my` in a package block no longer overwrites the enclosing lexical

`my $w = 1; package S { my $w = 5 }; say $w` printed `5`. A package block
runs over a block env tier, and at exit every bare name that the enclosing
env also had was treated as a write the body made to the enclosing lexical,
so it was copied out. Only an `our` declaration was told apart. The body's
`my $w` looked exactly like such a write.

The compiler now hands `PackageScope` the names the body's own scope declares,
and the exit keeps them in the body: they become the package's lexicals that
its subs close over. A plain write to an enclosing lexical, `$_` or `$/` still
reaches it, as on rakudo. Since the enclosing `@a`/`%h` no longer hides
behind the body's copy, `GetArrayVar` and `GetHashVar` now ask the
package-lexical store first, as `GetGlobal` already did. That lets a package
sub read its own `my @a` (#13428).
