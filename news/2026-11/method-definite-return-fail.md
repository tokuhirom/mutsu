# A `fail` in a method with a pinned return value hands back its Failure

`method plan-save(Str $f --> True) { fopen($f) || fail X::Libfftw3.new(...) }` returned `True`
even when the guard failed: the method path applied the signature's pinned return value
(`--> True`, `--> 42`) to the Failure a `fail` produced. Routines already let the Failure
through; the two method call paths (compiled slow and light) now do too, so Math::FFT::Libfftw3's
"fails if can't create output file" tests (t/05, t/06, t/07) match raku.
