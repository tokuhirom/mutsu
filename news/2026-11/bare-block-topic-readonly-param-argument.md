# A bare block called with a read-only parameter or loop alias gets a read-only `$_`

`my &dec = { --$_ }; sub f($x) { dec($x) }; f(2)` now dies "Cannot modify an
immutable Int", as rakudo does: the block's implicit `$_` is bound to the
argument itself, and a plain (non-`is copy`) parameter or a `for` loop alias is a
read-only binding. A writable variable (`my $v`, an `is copy` or `is rw`
parameter) is still written through.

Found by running the Iter::Able distribution (`t/29-for-each`).
