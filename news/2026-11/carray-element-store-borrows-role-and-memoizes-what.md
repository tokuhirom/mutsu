# A CArray element store memoizes the mixin's `.WHAT` and borrows its role definition

`@c[$_] = v` on a `CArray[num64]` (a role-mixed class instance) recomputed the mixin's `.WHAT`
composition key for the `::?CLASS:D:` invocant binding on every call, and cloned the whole
`RoleDef` (every method) several times to read one delegation entry or one method list.

The `.WHAT` of a class-instance mixin is now memoized by the identity of its shared override node
and its class (`ResolutionCaches::mixin_what_by_node`, the entry owns the node so its address
cannot be reused), and the role lookups on the mixin dispatch path
(`with_mixin_role_def`) borrow the `RoleDef` instead of cloning it.

Callgrind slope of `@c[$_] = 1.5e0` on a profiling build: 199k Ir per store before this change,
see the PR for the after figure. Refs #16215.
