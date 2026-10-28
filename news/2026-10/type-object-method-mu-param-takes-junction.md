# A method called on a type object binds a junction to a `Mu` parameter whole

`Class.method(any(...))` threaded the call over the junction even when the parameter is `Mu`; only instance receivers consulted the parameter types. A type-object receiver now resolves the method and applies the same rules (Path::Finder's `Path::Finder.skip-vcs`, which passes `any(...)` to `skip-dir(Mu $pattern)`).
