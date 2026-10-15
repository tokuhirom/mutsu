# An escaped closure calls the `my sub` of its own method

A `my sub` declared in a method body is registered by bare name, so two methods
declaring `gs` replaced each other and a closure escaping the first called the
second's `gs` (`b1b1` where Rakudo prints `a1b1`). Registration now remembers the
declaration site per name; once a second, different site declares the same name,
calls by that name use the Sub captured in the closure's env instead of the
registry's latest entry. Names declared at a single site keep the registry path
(and its `state` variables and wrap chains). Fixes #13550.
