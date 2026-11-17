# A closure in a module routine reads the routine's lexical, not the module's

A closure created inside a module routine that named one of the routine's own
variables — a `my`, or a pointy `for` loop parameter — resolved the name to the
module's same-named file-scope `my` (or package `our`) instead:

```raku
unit module M;
my $x = 'file';
our sub a is export { my @c; for 1, 2 -> $x { @c.push: { $x } }; @c.map({ .() }) }
our sub w is export { my $x = 1; my $c = { $x = 5 }; $c(); $x }
```

`a()` returned `(file file)` and `w()` returned `1`, while the closure's write
silently changed the module's `$x` to `5`. Rakudo gives `(1 2)` and `5`.

A by-name read in a module routine asks the module's own stores before the
frame env, since a bare env key there may belong to whoever loaded the
module; a closure's captured variables live in that env, so the module store
won. The compiler now records which free variables of a closure an enclosing
routine declares, and those names skip the module stores, both for reads and
for writes (#17466, ADR-12529 phase 3, slice 51).
