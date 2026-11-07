use Test;

# Found through Math::FFT::Libfftw3 (t/05-c2c): `[1.0+0i, 2.0+0i … 18.0+0i]`.
# A sequence with Complex seeds and a Complex endpoint is deduced and ended
# with Complex arithmetic.

plan 7;

is (1.0+0i, 2.0+0i … 5.0+0i).elems, 5, 'two Complex seeds, … endpoint';
is-deeply (1+0i, 2+0i … 4+0i).list, (1+0i, 2+0i, 3+0i, 4+0i), 'values are Complex';
is (1+0i, 2+0i ... 4+0i).elems, 4, '... spelling';
is-deeply (1+0i ... 3+0i).list, (1+0i, 2+0i, 3+0i), 'a lone seed steps by one';
is-deeply (5+0i, 4+0i ... 2+0i).list, (5+0i, 4+0i, 3+0i, 2+0i), 'counting down';
is-deeply (1+0i, 2+0i ...^ 4+0i).list, (1+0i, 2+0i, 3+0i), 'exclusive endpoint';
my @a = [1.0+0i, 2.0+0i … 18.0+0i];
is @a.elems, 18, 'the FFT test shape';
