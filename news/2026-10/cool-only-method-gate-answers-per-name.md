# The Cool-only method gate answers per name instead of building a name set

`is_cool_only_method` used to materialize the whole set of `Cool`-only builtin method names
(three catalog listings of `Cool`, `Any` and `Mu`) on the first instance method call of every
program. It now asks the catalog per name (`Cool` lists it, `Any`/`Mu` do not, plus the
`COOL_SUBTYPE_ONLY` names), which is the same answer without the set. On
`bench-grammar-parse` (profiling build, callgrind, second run) this takes 28.006M to 27.087M
instructions (-3.3%). Refs #14878.
