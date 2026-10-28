# A `for` body that only stores into a hash skips the topic alias

`for @k { %h{$_} = 1 }` bound `$_` to each element's own container, which promotes
every element of `@k` to a `ContainerRef` cell. The proof that a body cannot tell
the container from a copy of its value (#14495) was an allow-list that did not
include an associative store, so this common shape always paid for the alias.

The allow-list now admits `%h{...} = rhs` on a plain `%` variable and reads of one: a
hash element is never an array element, so the store cannot reach the source array.
Bodies that write `$_`, bind it, capture it in a closure or call user code keep the
alias, and `t/control/for-topic-hash-store-alias.t` pins both sides.

Measured on the `%h{$_} = 1 for @k` shape of #7549 (callgrind Ir, `profiling` build, 100 x 200
stores, startup included): 228.9M -> 165.8M (-27.6%); the store loop alone 217.2M -> 152.6M.
This is one slice of #7549's create-shape goal, not the goal: the store lane itself
(~3.6k Ir per store) and the base loop remain.
