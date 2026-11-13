# A user subscript on a typed array sees an Int index too

A `multi postcircumfix:<[ ]>(DataSlice @rows, Int $p)` candidate was never
called for a plain Int subscript: the VM's positional read fast path answered
`@rows[1]` from the array directly, so the user candidate only ever saw slices
and `*`. The fast path now steps aside for a typed array while a user
`postcircumfix:<[ ]>` is in scope (an untyped array's reads never pay the
probe), and the general path resolves the candidate.

`Dan`'s `df[*][1]` / `df[0..1][1]` (a row slice to a column Series) now work;
all four of its test files pass, matching raku.
