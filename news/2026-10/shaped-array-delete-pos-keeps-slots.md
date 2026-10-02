# `.DELETE-POS` on a shaped array keeps every slot

`my @s[4]; @s.ASSIGN-POS(1, 8); @s.DELETE-POS(1)` used to leave `@s` printing
as `[]` with `.elems` 0. `.DELETE-POS` trimmed trailing holes the way an
unshaped array does, and after the delete every slot of the shaped array was
a hole. A shaped array is fixed-size, so `.DELETE-POS` now only empties the
slot, just as the `:delete` adverb already did: `@s` prints as
`[(Any) (Any) (Any) (Any)]` (#10925).

Both delete forms now share one `ArrayData::trim_trailing_holes`, so the
"unshaped only" rule lives in one place.
