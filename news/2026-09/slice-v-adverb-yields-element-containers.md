# Slice `:v` yields the elements' containers

`@a[0,1]:v = 31, 32` and `%h<a b>:v = 5, 6` now write through to the source aggregate,
and an element of `my $l = @a[0,1]:v` aliases the array, as in Rakudo. A single-element
`:v` still returns a bare value. Missing keys/indices are still skipped and read-only
use prints plain values.
