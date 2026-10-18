# `my $x; $x.push(1)` vivifies from `Any`'s row

The autovivification of an undefined variable by `push`, `append`, `unshift` and `prepend` used to
be a name check in the VM's mutating-method opcode. It is now part of `Any`'s rows (ADR-11276
§9.171), so every entry that reaches the rows vivifies the same way. `$q.+push(1)` now vivifies once
and answers `([1],)`, as Rakudo does. A push onto an array held in a `$` variable answers the
`Array` (`[1, 2]`), not the item (`$[1, 2]`).
