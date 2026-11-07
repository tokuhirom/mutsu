# Math::FFT::Libfftw3 R2C no longer segfaults; ENTER sees plain declarations

`Math::FFT::Libfftw3::R2C.new(data => 1..18, dims => (6,3)).execute` crashed in `fftw_execute`.
Two independent bugs were behind it:

- `[*] @carray[0..*-2]` (and `[*] (@c[0], @c[1])`) folded the CArray elements' native references,
  which the numeric operators read as 0, so the module sized its output buffer at 0 and FFTW
  wrote into it. A reduction now folds the elements' values.
- `{ my $f; ENTER { $f = 1 }; say $f }` printed `(Any)`: the declaration statement, which runs
  after the ENTER phaser, reset the variable. A plain `my $x;` / `my @a;` / `my %h;` of a block,
  loop body, routine or closure now comes into existence before its ENTER phasers run.
