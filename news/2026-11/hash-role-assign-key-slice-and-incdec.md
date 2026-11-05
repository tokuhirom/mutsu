# A role-mixed Hash: key-list slice store and `++%h<k>` use the role's subscript methods

Hash::LRU's `is LRU` hashes mix a role that overrides `AT-KEY` / `ASSIGN-KEY`
into a Hash. Two element operations bypassed it:

- A key-list slice store, `$cache<a b c> = 1, 2, 3`, called `ASSIGN-KEY` once
  with the whole key list instead of once per key. It is now one call per key,
  each taking the next value (Nil past the end), as in rakudo.
- `++$cache<b>` / `$cache<c>--` read the backing storage directly, so the role's
  `AT-KEY` (which for an LRU also refreshes the key's recency) never ran and the
  element started from an undefined value. The increment now calls the role's
  `AT-KEY` / `AT-POS` once and stores the new value back through the container
  it returns, with no `ASSIGN-KEY` call, as rakudo does.

Pinned by `t/oo/role/hash-role-assign-key-slice-and-incdec.t`. Hash::LRU
`t/Cache-LRU.rakutest` now matches raku (38 ok).
