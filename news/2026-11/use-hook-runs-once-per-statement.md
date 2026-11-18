# A `use` in a loop or a routine runs `sub EXPORT` once

A module's `sub EXPORT` runs when its `use` statement is compiled, once per
statement, as on rakudo. mutsu used to run it again every time the block that
holds the `use` ran:

```raku
for ^3 { use HookModule; ... }   # EXPORT ran 3 times
sub f { use HookModule; ... }    # EXPORT ran on every call of f
```

Each `use` statement now carries the site the parse gave it, and the map the
hook returned when the parse loaded the module is kept under that site, so
every run of the statement installs the same map. A second `use` of a module
that is already loaded runs the hook at its own compile as well. Part of
ADR-17558 (slice 2c).
