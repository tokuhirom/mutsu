# A block-declared sub reaches its sibling subs after the block exited

```raku
my @c; for 1, 2 -> $v { sub a1 { "a$v" ~ b1() }; sub b1 { "b$v" }; @c.push(-> { a1() }) }
say @c.map({ $_() });   # (a1b1 a2b2)
```

ADR-12529 phase 2 slice 4 made a block's `sub` a lexical binding for the
closures nested in the block, but `a1`'s own body is a named routine body, so
its `b1()` still resolved by name: through the registry, which holds only the
latest activation's `b1` and none once the block exited ("Unknown function:
b1"). The activation capture a block sub's code object carries now also binds
the sibling routines its body (or a closure in it) calls, each as its own
capture of the same activation, and a by-name call in a frame linked to that
capture calls the binding. Mutually recursive siblings share the capture
through a cell (#13037).
