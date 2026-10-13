# buf16, blob8 and friends are the parameterized type objects

The terms `buf8`..`buf64` and `blob8`..`blob64` now evaluate to the
parameterized type object (`Buf[uint16]`, `Blob[uint8]`), as they are constants
bound to those types in Rakudo. `buf16.raku`, `.gist`, `.^name`, `.WHAT` and
`buf16 === Buf[uint16]` agree with `raku`; before, the alias survived as the
type object's name (#13314). The bareword resolution in
`push_bare_word_value_in` does the mapping, so every consumer sees the
canonical name.
