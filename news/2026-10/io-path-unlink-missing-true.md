# `IO::Path.unlink` of a missing file returns True

`"nope".IO.unlink` returned `False` in mutsu. Rakudo treats a file that
does not exist as already removed and returns `True`, the same rule the
`unlink` sub follows when it lists the path in its result. Only a real
failure, such as a directory target, still produces an `X::IO::Unlink`
Failure (#11468).
