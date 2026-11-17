# A sub that rebinds an `our` variable rebinds its lexical alias

`our %g` declares a package variable and a lexical alias of it. A sub that
rebound the name (`sub f($x) { %g := $x }`) changed the package variable
instead, and once the sub returned the declaring scope read its old, empty
container again:

```raku
our %g; our @a; our $s;
sub f($x) { %g := $x }
f({a => 1}); say %g;      # mutsu: {}       raku: {a => 1}
say %OUR::g;              # mutsu: {a => 1}  raku: {}
```

A mainline sub now captures an `our` declaration's alias the way it captures
a `my` (ADR-0024), so the rebind lands in the binding the declaring scope and
every other sub read, and `%OUR::g` keeps the package's own container, as in
rakudo. Two related fixes came with it: `$OUR::s = 7` stores through the
package variable's cell, so the alias sees it (it used to replace the bare
entry with a plain value), and `@OUR::a` / `%OUR::h` read the package
variable rather than the bare-name alias (#17508, ADR-12529 phase 3).
