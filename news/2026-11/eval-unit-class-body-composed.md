# EVAL of a `unit class` body runs after the class is composed

An EVAL'd unit now gets the same declaration-phase passes as the main program:
variable traits are lifted to BEGIN time and a class body's bare statements are
split from its declaration, so `EVAL Q[unit class Foo; $saw = Foo.^is_composed]`
sees a composed class. The body statements keep writing the caller's lexicals
(a bare name in the run-time part of a split body is no longer qualified with
the package unless the body declared it `our`), and a plain (non-multi)
`sub trait_mod:<is>` is now seen as a handler by variable traits (#17192).
