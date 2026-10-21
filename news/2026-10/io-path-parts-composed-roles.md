# `IO::Path::Parts` lists its roles and iterates through `Iterable`

`IO::Path::Parts.^roles` was empty although the class does `Iterable`,
`Associative` and `Positional`, and `.hyper`/`.race`/`.lazy` were missing.
The catalog's roles are read for it now, and `Iterable`'s rows iterate the
three part pairs the way `for` and `Any`'s list methods already did.
