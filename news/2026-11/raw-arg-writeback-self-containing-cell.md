# An `is raw` argument no longer makes a rebindable variable contain itself

A variable a closure rebinds with `:=` holds a binding cell. Passing it to an
`is raw` or `is rw` parameter binds the parameter to that very cell, and since
#17483 the call-return writeback stores through the binding's *value* cell. Its
identity check still compared the incoming binding cell with that value cell, so
it missed, and the value cell received the binding's content: a reference to
itself. Every later read of the variable and every `start` (which walks the
spawning frame's variables for IO handles) then looped or overflowed the stack.

```raku
sub take-raw($x is raw) { 1 }
sub f() { my $p = 1; my &r = { $p := 2 }; take-raw($p); await start { $p } }
say f();   # 1; mutsu hung
```

ParaSeq's batches pass exactly such variables to `is raw` parameters, so nearly
every one of its test files hung and then crashed. The writeback now resolves the
incoming cell to its value cell before comparing, and the spawn-time handle walk
unwraps container layers in a bounded loop instead of one stack frame per layer.
