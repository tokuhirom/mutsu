# A typed declaration with an initializer keeps its value as a call argument

`ok my IntTree(Str) $tree = "1(2)(3)"` left `$tree` the `IntTree` type object, and
so did `f(my Int $u = 5)` for any parameter that is not `is raw` (#16291). The
declaration stores its initializer into the variable's slot and registers the
type afterwards; with the variable living only in its slot, `SetVarType` found
no env entry, took that for an unset variable and seeded it with the type
object, which the call's writeback then put over the value. `SetVarType` no
longer seeds when the slot already holds a value. BTree's
`t/simple-int-trees.rakutest` now matches rakudo (5 of 5). Pinned by
`t/types/coercion/typed-declaration-assigned-in-call-argument.t`.
