# `@a.head` / `@a.tail` hand back the element itself

An Array's argless `.head` and `.tail` now return the element's container
(the same shared cell `my $x := @a[i]` binds), as rakudo's do, so
`$_ = .Int + 1 with @parts.tail` and `my $x := @a.head` write into the array.
A `given`/`with` whose topic expression turns out to be a container at run
time is writable even when the compiler could not see that statically. A
`List`'s elements stay immutable, and an empty Array still answers `Nil`.

Found via Version::Nginx, whose `as-generic-range` bumps the last component
of a version this way; both of its test files now pass.
