# The first call of a sub inside a `do { }` block no longer replaces the enclosing topic

`for <x> { do { give(); say $_.raku } }` printed `Any` where rakudo prints `"x"` (#15955), but only the
first time the sub was called, and only for a call statement that was not the block's last. The slow call
path (a routine's first call) took a fast exit when the callee's env and the saved caller env were
`ptr_eq`. That test compares the overlay map only, and every empty overlay is the one shared singleton: a
callee that wrote nothing (a `scoped_child` frame root, `sub noop { }`) equals a caller env that is itself an empty
tier, which is exactly the block tier of a `do { }` in a `for` body. The skipped restore left the callee's
frame root, which owns `$_`, installed in the caller. The exit now also requires that the current env is
no frame root. Pinned by `t/control/do-block-call-statement-keeps-topic.t`.

Not fixed here, filed separately: a `require`/`need` of a module that declares a class replaces the
enclosing `for`/`given` topic with the loaded type object (the other half of the Red `schema(<Post Person>)` shape).
