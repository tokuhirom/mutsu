# EVAL of a lowered RakuAST tree sees what its `use` statements import

`EVAL($ast)` of a tree that holds a `use` reported `Undeclared routine` for the routines the module
exports, although the same text runs fine as a string `EVAL`: a string is parsed in place, which fills the
parser's import table that the CHECK-time undeclared-routine scan asks, and a tree parsed earlier
elsewhere left that table empty. The lowered-tree entry now registers the exports of the tree's `use`
statements (and its static `use lib` paths) in a scope of their own before validating, and runs through
the same `validate_and_run_eval_unit` as a string `EVAL` (its `use`s are pre-run, its import region is
opened and closed). When a `use` of a module that may import names follows a `use lib` in the tree,
nothing about those imports is known before the unit runs, so the two name checks give up, as the
mainline check does.

Found by the `EVAL(slurp($f).AST)` ratchet (#15759): of the 91 `t/` files that failed with
`Undeclared routine` or `Undeclared name` through it, 34 now pass. The rest are other causes, mostly
files that read `$?FILE`, which names the driver, not the test.

Sharing the string-EVAL pipeline exposed four gaps that a warm precompile cache hid, and CI
(always cold) caught them through the `EVAL($ast)` ratchet: an exported type of a pre-run module
in a signature (`use NativeCall; sub f(Pointer $p)`), a bareword bound in the unit's own import
set (`use Telemetry; T<wallclock>`), a nested `use lib` ahead of a later `use`, and
`array[uint8].new` read as a routine call. All four are fixed for string `EVAL` as well.
`t/modules/compunit/lib-nested-use-begin-time.t` leaves the ratchet list until #15882.
