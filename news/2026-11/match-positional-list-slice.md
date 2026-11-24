# `$m[0, 1]` slices a Match's positional captures

A list slice of a Match's positional captures answered `(Nil, Nil)`:
`my $m = "ab12" ~~ / (<[a..z]>+) (\d+) /; say $m[0, 1]` read the indices as
*named* capture keys, because the subscript arm that handles an `Array` index
on a Match did not look at whether the subscript was `[ ]` or `{ }`. A
positional slice now reads each index from the positional captures (`Nil` past
the end), as a single index and a Range slice already did, while `$m{<a b>}`
still reads the named ones (#19154).
