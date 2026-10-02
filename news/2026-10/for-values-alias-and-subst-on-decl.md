# `for values %h` aliasing and `(my $x = ...) ~~ s///`

Two gaps found while making the `Color` distribution pass. `for values %h` /
`clip-to 0, $_, 255 for values %r` now aliases the hash or array values like
`%h.values` does, so `is rw` parameters write through. `( my $o = $s ) ~~ s/d$//`
now substitutes in the newly declared variable instead of a temporary copy.
All 8 `Color` test files now pass under mutsu.
