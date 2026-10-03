# A `:D` scalar declared before a BEGIN-time effect no longer dies

`my Int:D $x = 3;` followed by a `BEGIN` block or a `use` statement died with
`Type check failed in assignment to $x; expected Int:D but got Int (Int)`
before any of the unit ran. The BEGIN prologue (ADR-0134) splits such a
declaration into a static half, which a BEGIN can observe, and a run-time
assignment of the initializer. The static half kept the `:D` constraint and
the explicit-initializer mark but held `Nil`, so its own store failed the
definedness check.

The static half of a `:D` scalar now holds the nominal type object, as in
Rakudo (`my Int:D $x = 3; BEGIN say $x.raku` prints `Int`). The compiler
stores it before it registers the constraint, the same order an `is default`
trait already uses. The run-time assignment is still checked, so
`my Int:D $x = Int; BEGIN {}` and a later `$x = Nil` still die. A BEGIN nested
in a block keeps the variable's static state in a cell; that cell now holds
the type object without the constraint.

Found via the `Usage::Utils` distribution, whose module starts with
`my Bool:D $debug = False;` ahead of its `use` lines. Both of its test files
now pass under mutsu.
