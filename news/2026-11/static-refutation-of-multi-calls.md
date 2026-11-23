# A refuted multi call fails at CHECK time too

`say 1; multi f(Int $x) {}; multi f(Num $x) {}; f("a")` printed `1` and
then died with "Cannot resolve caller f(Str:D)". rakudo rejects it at CHECK
time:

```
Calling f(Str) will never work with any of these multi signatures:
    (Int $x)
    (Num $x)
```

The CHECK-time refutation pass (#18874) now judges a unit's top-level
`multi` candidates as rakudo's dispatcher does. A call with literal arguments
is refuted when every candidate rejects it. A candidate's default, a `:D`
smiley and an optional named parameter do not save it, as they do not in
rakudo.

The candidates must be all there is, so the pass skips a multi in two cases:

- it shares its name with a core or `Test` routine (`multi min(Int $a)` adds
  to CORE's `min`);
- the unit imports a module, which could add candidates of its own.
