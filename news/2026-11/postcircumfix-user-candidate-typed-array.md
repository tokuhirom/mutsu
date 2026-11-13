# A user postcircumfix candidate for a typed array answers element reads

A user `multi postcircumfix:<[ ]>(Int @a, Int $p)` (or one typed with a user
class, `DataSlice @aods`) was ignored for an ordinary `@a[1]` read: the
element-read fast path answered every in-range `Int` index of a real Array
before the general path could consult user candidates, so only an explicit
`postcircumfix:<[ ]>(@a, 1)` call reached them. The fast path now declines
when a user subscript routine could answer (an open operator scope, an
exported operator family, or a `postcircumfix` routine in the function
table), leaving the read to the general path's candidate resolution.

This made Dan's `df[*][1]` / `df[0..1][1]` series selection work: all four
of Dan's test files now match rakudo.
