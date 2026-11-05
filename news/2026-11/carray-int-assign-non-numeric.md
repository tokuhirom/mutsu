# Assigning a non-numeric Str into an integer CArray element dies

`my $i = CArray[int32].new; $i[0] = "x"` stored 0 silently because
`nqp::unbox_i` answered the C address (0) for any object that carried none.
It now refuses a `Str`, `Num`, `Bool`, rational or `Failure` the way MoarVM
does ("This type cannot unbox to a native integer"), so the assignment raises
`X::AdHoc` like rakudo. Pointers, CArrays and Bufs still unbox to their
address (#16420).
