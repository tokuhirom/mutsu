# An imported routine's light call skips the argument-source decode

The positional-light call of a routine bound by a lexical `use` (#14371) asked the
call site to decode its argument-source table (names, slot map refill) only to learn whether
it spreads a `|EXPR`, and scanned each argument with three separate views for
Pair/Junction. It now asks the site for the spread bit alone and classifies each argument
with one view.

Measured (profiling build, callgrind second run, slope 1,000 -> 3,000 iterations of
`for ^N { $s = inc($s) }`): imported 5,697 -> 5,514 Ir per iteration, against 5,268 for
`inc` declared in the program: +4.7%, inside the issue's 5% close condition.
