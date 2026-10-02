# An omitted optional subset parameter checks its implicit default

`subset S of Str where * eq "x"; sub f(S :$s) {}; f()` used to run `f`
silently. An unpassed optional parameter bound the subset's own type object
(`S`), which trivially satisfies `S`, so the predicate never ran. Rakudo binds
the parameter's *nominal* type object instead — the subset's refinee, `Str`
here, and `Int` for `UInt` — and checks the predicate against it, so the call
dies with "Constraint type check failed in binding to parameter '$s'; expected
S but got Str (Str)".

mutsu now does the same on every binding path: the sub binder (positional and
named), the multi-dispatch candidate check (so such a candidate is no longer
selected for a bare call), and the compiled-method fast path. A `MAIN(S :$s)`
run without arguments now prints the usage instead of running.

When the rejected value is an omitted parameter's implicit default — through a
subset or an explicit `where` — the error carries Rakudo's explanation ("The
parameter is optional and was not passed an argument, ..."). Subset constraint
failures also spell the value the way `.raku` does (`got Str ("a")`, not
`got Str (a)`), matching the `where` failure message.
