# A twigil-less attribute bound to a value is readonly

`has $x` bound to a plain value in a method (`$x := 42`) stayed assignable:
a later `$x = 5` replaced the value, in another method or the same one.
`$!x := 42` already left the attribute without a container. The twigil-less
spelling, which binds through the attribute's sigilless alias, now does too.
It seats the value in a readonly binding cell, marks the attribute bound,
and makes the method's own names for it readonly, so the assignment dies as
in rakudo (#18365).
