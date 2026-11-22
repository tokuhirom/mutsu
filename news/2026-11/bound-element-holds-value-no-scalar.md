# An element bound to a value or an Array holds it with no Scalar

Rakudo's `BIND-KEY`/`BIND-POS` of a value or an `@`/`%` variable store the
object itself in the element, with no container (#18566). So `%h<a> := @s`
reads and iterates as that Array, `.VAR` answers `Array`, and `%h<a> = 4` dies
with "Cannot assign to an immutable value". mutsu got each case wrong in a
different way:

- `%h<a> := @s` shared `@s`'s variable cell. `%h<a> = 4` then reassigned `@s`
  itself to `4`.
- The chained `%g<a><b> := @s` itemized the Array: it read as `$[1, 2]`,
  and `for` saw one item.
- Any value bind (`%h<a> := [1, 2]`, `@a[0] := (1, 2)`) answered `Scalar` from
  `.VAR`. `for %h<a> { }` then took one item and died writing it back.

A bound element is still a read-only `Immutable` cell, which is what refuses
`=`. But that cell now stands for "no container":

- `Value::bound_element` makes a plain (not itemized) holder.
- An `@`/`%` source binds its aggregate the same way, while an `=` share
  (`@aoa[0] = @row`) keeps its shared cell.
- `.VAR` on such an element answers the value. This is what makes `for`
  iterate it and skip the writeback.
- A positional `.VAR` subscript returns the cell, so it can tell.
- `=:=` compares the bound aggregate's identity.
