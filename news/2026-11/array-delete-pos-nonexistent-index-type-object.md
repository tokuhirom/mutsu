# `Array.DELETE-POS` past the end answers the element type object

`@a.DELETE-POS(4)` on a two-element Array answered `Nil`; rakudo answers the
type object a hole reads (`Any`, or `Int` for `my Int @a`). The shared
`delete_pos_in_array_data` now returns that type object when the index is past
the end, so the method, the `:delete` adverb and the multi-dimensional walk's
innermost level agree. Pinned by
`t/collections/array/array-delete-pos-nonexistent-index-type-object.t`
(tests 20 and 41 of `roast/S32-basics/xxPOS.t`).
