# A `.grep` callback allocates nothing per element

`.grep({ $_ %% 3 })` runs its callback through the inline loop plan. For every element that loop
built a one-element `Vec` to hold the topic (for an arity-1 callback, which needs no `Vec`) and a
fresh `String` naming the topic's source variable, which is the same name every time. That is two
heap allocations per grep element.

The loop now holds the topic element by value, and sets the topic source only when it does not
already name the key; the body may still change it between elements, which the check covers.

Measured with callgrind on `benchmarks/array-ops.raku` (#19323): 200,000 fewer allocations (two
per grep element, now none), and 4.3% fewer instructions (~330 per grep element). What remains per
callback is the name-keyed binding of `$_`, tracked in #19323.
