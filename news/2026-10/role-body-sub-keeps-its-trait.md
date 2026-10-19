# A role's own `sub` keeps its traits when the role is composed

Composing a role into a class ran the role body's `sub` declarations a
second time, under the composing scope's package, so a trait on such a sub
was applied to a second copy and `f()` from a composed method reached the
wrong one. A trait that wraps the routine was lost that way: NativeCall's
`is native`, for one, made a role's native sub run its `{ * }` body and die
with "expected Str but got Whatever" (Gnome::Gio's `Icon` role, #13645).
The role body's subs now register in the role's own package on every
composition, and they are no longer callable from outside the role.
