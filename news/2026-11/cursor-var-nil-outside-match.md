# `$¢` is Nil outside a match

`say $¢` printed `(Any)` outside a regex; rakudo answers `Nil`, as for `$/` (found by the
raku-doc differential sweep, `Type/Match.rakudoc`). Inside a regex code block it is still the
current match state.
