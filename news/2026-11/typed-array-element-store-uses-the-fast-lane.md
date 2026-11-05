# An element store into a typed array of a plain builtin type takes the compiled lane

The compiled array element-store lane declined every array that carried element
type metadata (`my Int @a`) and every declaration with a type record, so a typed
array's `@a[$i] = $v` always ran the general `exec_index_assign_expr_named_op_inner`
(~26,500 instructions per iteration against ~4,300 for an untyped one, #14702).
The lane now serves a typed array whose embedded element type is `Int`, `Str`,
`Num` or `Bool` and whose stored value is exactly that type (a tag test, so no
constraint machinery runs): the array must carry no key type, no default and no
native backing, its declared container type must be `Array[<the same type>]`, and
the declaration's by-name record, when there is one, must say the same type.
Everything else keeps the full path: a value the constraint would reject (the
error is the full path's to raise), a coercion, a subset, a role or class element
type, a native `int`/`num`/`str` array. Pinned by
`t/collections/typed-array-element-store-lane.t`.
