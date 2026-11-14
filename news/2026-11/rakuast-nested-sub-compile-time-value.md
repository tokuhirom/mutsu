# CHECK-time `compile-time-value` of a sub declared in a nested block

`RakuAST::Sub.compile-time-value` of a `sub` declared inside a nested block
answered `Any` when asked at CHECK time. mutsu installs such a routine when
its block runs, so it was neither a lexical nor in the function table yet.
rakudo answers the routine's static code object, whose `static_id` equals that
of the `&name` the block binds later. Red's CHECK compiler keeps that value to
validate a helper call before inlining it, and t/091 died binding `Any` to a
`Code:D` parameter.

The value is now the routine compiled from the declaration itself, as an
anonymous sub so that nothing is installed. It carries the declaration's name,
the unit's package, file and declaration line. A resolution's declaration is
held without its body, so that case compiles the declaration's own source
text. `Code.static_id` now keys every named routine on that declaration
identity (it did so only for compiled ones), so the CHECK-time value and the
bound routine compare equal.

With this, Red t/091 runs to its plan: 6 of its 8 subtests pass, where it used
to die in the third.
