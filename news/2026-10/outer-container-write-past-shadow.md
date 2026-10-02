# `@OUTER::a` and `%OUTER::h` reach the outer container past a shadow

Writes through `OUTER::` to an array or hash now reach the container
`OUTER::` names, even when a scope in between declares its own `@a` / `%h`.
This covers a whole-container store (`@OUTER::a = 3, 4`), a rebind
(`@OUTER::a := [...]`), a mutating method call (`@OUTER::a.push(5)`,
`@OUTER::a .= sort`) and an element store (`@OUTER::a[0] = 9`,
`%OUTER::h<k> = v`), both in the same frame and across a routine boundary
(#10857). All of them used to be lost. A plain read of `@OUTER::a` /
`%OUTER::h` was not even lexical: it compiled to a by-name lookup of the
literal `@OUTER::a` key and yielded an empty container. It now resolves like
`$OUTER::x`.

Containers take the route #10827 built for scalars. The key the shared cell is
published under keeps the sigil in front (`@__mutsu_outer::<scope>:<name>`),
so a store through it keeps its list-assignment semantics and a method call's
write-back lands in the cell. An element store needs no key: it stores into the
container the lexical read yields, which is reference-shared.

The same work fixed a crash: `$OUTER::x = $OUTER::x.succ` and
`$OUTER::x .= succ` past a shadow overflowed the stack. `GetOuterVar` pushed
the shared cell itself, and the method call wrote its result back into that
same cell, so the cell ended up containing itself. The read now yields the
value.

A multi-level element store whose root is an expression rather than a variable
name does not autovivify (`(%h)<a><b> = 1` leaves `%h` empty). That also covers
`%OUTER::h<a><b> = 1`, and is tracked as #10900.
