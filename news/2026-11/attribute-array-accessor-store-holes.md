# A store through an `@` attribute's accessor keeps its holes

`$obj.d[2] = 1` on a `has Int @.d` holding one element grew the array, but
`array_grow_to` did not record the skipped slot as a hole. So
`$obj.d[1]:exists` was True and `.List` read `Int` there, where a store
into a variable (`@a[2] = 1`) leaves `:exists` False and `.List` reads
`Nil`, as on rakudo. The grown slots are now recorded as holes, a later
store into one marks it present, and a whole-container replacement keeps
the source's hole set.

Assigning a list with a wrong element to a boxed typed attribute array
(`$obj.d = (1, Any, 3)`) now reports rakudo's X::TypeCheck::Assignment
"Type check failed for an element of @!d; expected Int but got Any (Any)"
instead of a native-unboxing message. Test:
`t/oo/attribute/attribute-array-accessor-store-holes.t` (#17778).
