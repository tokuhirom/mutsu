# `$x ~~ SomeClass` honours a user `ACCEPTS` on the class

A class that declares `method ACCEPTS` is now asked when its type object is
the matcher: `"yes" ~~ Picky`, `when Picky`, `.grep(Picky)` and a variable
holding the type object all dispatch `Picky.ACCEPTS($topic)`, as rakudo does,
including an `ACCEPTS` inherited from a parent class. Before, a type-object
right side always took the nominal type test.

A multi candidate that does not bind (an `Int`-only candidate against a `Str`
topic, or a `:D`-invocant candidate against the type object) leaves the
nominal test in place, and a role's own `ACCEPTS` is still not consulted for
the role's type object.
