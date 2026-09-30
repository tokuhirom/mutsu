# A defaulted array declared as an expression keeps its default and its elements

`(my @a is default(D) = LIST)` and `(state @a is default(D) = LIST)` used in
expression position now take the same default-first route as the statement
form: the default-aware container is created before the initializer runs and
the initializer is stored into it. The value of the expression is that
container, so a `Nil` item becomes the default while an explicit `Any` stays
`Any`. Previously the `my` form substituted the default into the explicit
`Any` (`$[1, 1]`) and the `state` form returned the raw initializer list
(`$[Nil, Any]`).

The `state` form also sits behind the state-initialization guard, as the
statement form does, so the pre-created container and the initializer store do
not overwrite the persisted value on later calls (every store to a `state`
slot is published to the state store).

Sharing the default-first helper between the two forms exposed a stray `Pop`
after its `SetLocal`, which already consumes the value it stores. In statement
position that `Pop` discarded whatever the enclosing expression had pushed, so
`10 + do { my @a is default(1) = 1, 2; 5 }` panicked in the interpreter and
`@r[0] = my @a is default(9) = 1, 2` lost its element index. The `Pop` is gone.

Closes #10318.
