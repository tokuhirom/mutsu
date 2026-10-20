# A mixed-in parametric role's `gist` wins when the role has a plain candidate

`1 but H['pt']` answers `H`'s own `gist`, `Str` and other methods ahead of the
built-in ones, when `H` declares them. mutsu looked those methods up on the
role registered under the bare name `H`. When the file also declares a plain
`role H { ... }` next to `role H[\u] { ... }`, that lookup found the plain
candidate, which has no `gist`, so the built-in won. `(1 but H['pt']).gist`
printed `1`.

The check now reads the candidate the mixin was made from, the same way
attribute lookups on a mixin already did. CSS::Units declares
`method gist {self ~ units}` in its parametric candidate next to a plain one,
so `1pt.gist` is `1pt` again.
