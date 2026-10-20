# A coercion to a subset goes through the subset's refinee

`my subset S of N where ...; my S() $x = $v` now coerces the way Rakudo does:

- A value already in the subset is kept as it is.
- Any other value goes through the refinee's own coercion: `N.COERCE`, or
  `Int()` for `subset P of Int`.
- A result that falls outside the subset dies with `X::Coerce::Impossible`.

Before this change mutsu looked for a coercion method on the subset itself,
found none, and always died with "Impossible coercion".

PDF's `t/indobj-name.t` coerces a Str-valued enum value through
`my MyName() $baz`, where `MyName` is a subset of the `PDF::COS::Name` role,
and now passes all its tests (#12907).
