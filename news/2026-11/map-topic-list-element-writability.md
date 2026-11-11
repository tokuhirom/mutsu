# A `.map` topic over a `List` is writable exactly where its element is a container

raku binds a `.map`/`.grep` callback's `$_` to the source element, so
`$_ = ...` is legal exactly when that element is a container. mutsu decided
this from the receiver's syntax at the call site, and treated any receiver it
could not classify as writable. So `sub a(\it) { it.map({ --$_ }) }` called
with `(1, 2)` silently decremented copies where raku dies with "Cannot resolve
caller prefix:<-->" (Iter::Able's `for-each` test, #16949).

A plain `$` variable or a sigilless term no longer counts as a known receiver:
the run time decides, and for a `List` source it now decides per element. A
container element (`($x, $y).map(&f)`) keeps `$_` writable, which previously
died for a callback passed as a value, and a bare one makes it immutable.
`.grep` over a `List` takes the same per-element path. Arrays, `@a.list`, and
anything written directly at the call site behave as before. All 33 of
Iter::Able's test files now match rakudo.
