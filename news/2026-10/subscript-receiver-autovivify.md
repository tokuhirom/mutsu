# A subscript element vivifies through its own location

`@a[1].ASSIGN-KEY(...)`, `%h<k>.BIND-KEY(...)`, `f(@c)[2].push(5)`, `@e[1]."$m"(4)` and
`$obj.rw-attr.push(1)` now vivify an undefined receiver the way Rakudo's `Any:U` candidates do
(#13823, ADR-13823). The subscript receiver producer (`IndexInvocantRef { vivify }`) hands the call
the element's location - the cell of an existing slot or the deferred token of a missing one - and
`CallMethod` builds a `ReceiverPlace::Cell` from it. The `Any` rows store a fresh `Array`/`Hash`
through the cell (obeying the element's type constraint) and grow it on a detached place over the
same shared node, so the push family no longer needs the name-based `IndexElemAutoviv` path for a
single subscript. A type-object element with its own `push` (`@a[0] = P; @a[0].push(1)`) reaches that
method instead of being replaced by an `Array`.
