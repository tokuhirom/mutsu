# `state` in a `when` / `default` body restarts on every execution

A `when` or `default` body is a block the enclosing block re-clones each time it
runs, so a `state` declared in it re-initialises on every call of the routine
(Rakudo: `sub f($x) { given $x { when 1 { state $e = 5; $e++ } } }` returns `5`
every time). mutsu already did this for `if`/`unless` branches, bare blocks and
loop bodies via `OpCode::ResetStateLocals`, but the `when`/`default` arms never
emitted it. The state store therefore kept a stale entry, and when the routine
recursed (the `when` ran at depth 2 while the outer frame's exit-time sync wrote
an unset slot back) a later call read an empty `state %known`.

Found working Template::HAML, whose `DirectCodegen.validate-node` recurses over
a tree with `state %known = ...` inside `when Statement`: every direct-emit
render died with `unknown Statement kind: expression`. 19 of its test files now
run to the plan or further (`t/0230`, `0240`, `0260`, `0280`, `0300`, `0330`,
`0360`, `0410`, `0450`, `0710`, `0730`, `0740`, `0760`, `0800`, ...). Pinned by
`t/control/when-body-state-restarts.t`.
