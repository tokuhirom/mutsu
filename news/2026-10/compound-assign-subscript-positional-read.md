# Compound assignment on a user positional subscript reads positionally

`$obj[0] //= 0`, `||=`, `&&=` and `+=` expanded to a read of the element through
the associative `{}` form unless the target was an `@`-variable. For an object
with its own `AT-POS` that read found nothing (or died with "does not support
associative indexing"), so `//=` overwrote a defined element. PDF's
`self.Index[0] //= 0` reset a cross-reference stream's `/Index`, which broke
re-decoding it (#14083). The read is positional unless the target is itself a
subscript, where the lenient read still autovivifies an intermediate.
