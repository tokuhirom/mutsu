# `state` and `our` arrays apply `is default` before their initializer

`state @s is default(7) = Nil, Any` and `our @p is default(7) = Nil, Any`
now keep the explicit `Any` element, as `my` declarations already did: the
default-first route (the container gets its default before the initializer
is stored into it) is no longer limited to `my`.

For `state`, the whole sequence sits behind the state guard, and the
default-aware container — not the raw initializer list — is what
`StateVarInit` persists, so the initializer still runs on the first entry
only and the default survives later entries. A typed `state Int @a is
default(...) = ...` also registers its element type before `StateVarInit`,
so it is an `Array[Int]` rather than a plain `Array`. For `our`, the package
variable is published from the same default-aware container instead of a
second copy of the raw initializer list (#10256).
