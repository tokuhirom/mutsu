# A built-in method on an inline receiver takes the method-table lane

The method-table site lane (ADR-11276 §2.4/§2.5) answered a built-in method
called on a variable (`$s.lc`, `@a.elems`) from its row, past the dozens of
probes the full `CallMethodMut` path runs first. A call on an inline receiver
— `$s.substr(0, 2).lc`, `'a b'.words.join('-')`, a grammar action's
`$<a>.ast` — compiles to the plain `CallMethod`, which the lane did not serve,
so it walked the whole chain on every call. ADR-11276 recorded that as the
next step when the lane landed.

The plain opcode now asks the same lane first (`vm_call_method_site_lane`),
with the same guard and the same per-site memo. It adds the checks the plain
path makes by name before its native probe: `.perl` (renamed to `.raku`),
`.throw` (a resumable throw site), a native array's retyped answer, and the
`of`/stash arms. Only an immutable scalar (`Str`, `Int`, `Num`, `Rat`, `FatRat`,
`Complex`, `Bool`) or a `Match` is admitted as an inline receiver: the plain
path streams or reifies a deferred `Seq` that a collection holds (a lazy
`.keys`, a `Pair` whose value is a `.map`) before its native probe, and the
debug cross-check caught the lane answering such receivers without that. In debug builds the full path still runs for every pure row
and must agree, so the TAP suite checks every hit.
