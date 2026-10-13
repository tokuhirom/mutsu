# A coercion into a role calls the role's `COERCE` method

`sub f(R() $x)` with `role R { method COERCE(Str:D $v) { ... } }` died with
`Impossible coercion from 'Str' into 'R'`, because the coercion protocol only
looked for a `COERCE` method on a class target. PDF's `role PDF::IO` declares a
`proto method COERCE` with one `multi` per source type, and `PDF::IO() $x =
$path` is how the distribution opens its input. Its `t/io.t` and
`t/write-pdf.t` stopped at that line.

A role target that declares `COERCE` now gets the call on the role's pun, as
in Rakudo. The result has to do the role, and when no candidate accepts the
value, the coercion is still `X::Coerce::Impossible`.
