# `.map` on an `@` variable bound to a CArray, and sequences of Complex numbers

`my @y := CArray[num64].new: ...; @y.map(-> $r, $i { ... })` (and
`@!out.map(...)` on an attribute bound that way) treated the CArray as ONE item,
so a two-argument pointy block died with "Too few positionals" and
`@y.map(* + 1)` answered `(1)`. The rw-writeback `map` path now iterates an
instance (or role-mixed instance) receiver through the ordinary dispatcher, as
`$z.map` already did. Separately, `1.0+0i, 2.0+0i … 18.0+0i` (Complex seeds and
a Complex endpoint) is deduced with Complex arithmetic instead of running on as
real numbers. Both found through Math::FFT::Libfftw3's `C2C.execute` and its
tests. A 2D r2c transform still crashes with SIGSEGV (tracked in the PR body).
