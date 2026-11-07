# A reduction folds the VALUES of CArray elements, not their native references: `[*]` over a
# slice or a list of CArray elements was 0, which made Math::FFT::Libfftw3's output buffer empty
# and crashed fftw_execute (#17010).
use Test;
use NativeCall;
plan 8;

my @d := CArray[int32].new: (6, 3);
is ([*] @d[0..*-2]), 6, '[*] of a one-element CArray slice';
is ([*] @d[0, 1]), 18, '[*] of a two-element CArray slice';
is ([*] (@d[0], @d[1])), 18, '[*] of a literal list of CArray elements';
is ([+] (@d[0], @d[1])), 9, '[+] of the same list';
is ([*] (@d[0],)), 6, 'a one-element list';
is ([~] (@d[0],)), '6', '[~] of one element';
is ([max] (@d[0], @d[1])), 6, '[max]';
is (([*] @d[0..*-2]) * (@d[*-1] / 2 + 1).floor * 2), 24, 'the FFTW r2c output size';
