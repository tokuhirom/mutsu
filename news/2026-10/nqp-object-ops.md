# The object-model `nqp::` ops

Ten of the object-model `nqp::` ops are implemented (#11499): `how`,
`how_nd`, `who`, `what_nd`, `reprname`, `objectid`, `findmethod`,
`tryfindmethod`, `callmethod` and `call`.

In Rakudo, `.HOW`, `.WHO`, `.WHAT`, `.REPR` and `.WHERE` are themselves these
ops applied to `self`. So each op compiles to the method call that gives the
same answer, as `nqp::where` already compiled to `.WHERE`:

- `nqp::how($o)` is `$o.HOW`, and `nqp::how($o) =:= Int.HOW` holds.
- The "no decont" forms look at the container through `.VAR`, so
  `nqp::what_nd($x)` is `Scalar` for a variable.
- `objectid` is `.WHERE`, because mutsu's GC never moves an object.
- `callmethod` is a dynamic method call (`$o."$name"(...)`), and `call` is an
  ordinary invocation.

`findmethod` and `tryfindmethod` go through the same resolver as
`.^find_method`, so the two cannot disagree. On a miss, `findmethod` dies and
`tryfindmethod` answers null.

The `nqp::` coverage table now stands at 477 of 577 ops. `rebless`, `setwho`,
`bind` and `bindcomp` remain.
