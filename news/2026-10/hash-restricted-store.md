# Hash::Restricted passes: native STORE and assignment to mixed-in containers

Hash::Restricted's test file now passes 32/32 under mutsu (was 24/32, and a
script whose routine captured a restricted hash hung outright).

- `Hash.STORE` and `Array.STORE` exist as native methods. They re-initialize
  the container in place, so every alias sees the new contents, and return
  it. A role's own `STORE` reaches them through `callsame`.
- Assigning to a hash or array that has a role mixed in (`%h does R`, the
  `is restricted` trait) is now the container's `STORE`: a role `STORE` runs,
  and the container keeps its role. Before, the assignment replaced the value
  with a plain Hash, dropping the restriction, and through a `for` loop
  parameter it did not reach the caller's hash at all.
- `trait_mod:<does>` applied to a variable that a routine captures composes
  the role onto the value its shared cell holds. It wrapped the cell itself,
  storing a cycle into it that deadlocked the next read.
