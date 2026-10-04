use Test;

plan 13;

# The four Complex-owned methods must agree on literal and variable receivers.
my $z = Complex.new(3, -4);
is $z.re, 3, 'variable real component';
is $z.im, -4, 'variable imaginary component';
is-deeply $z.reals, (3e0, -4e0), 'both components preserve their order';
is $z.conj, 3+4i, 'variable conjugate';
is Complex.new(-2, 5).re, -2, 'inline real component';
is Complex.new(-2, 5).im, 5, 'inline imaginary component';
is-deeply Complex.new(-2, 5).reals, (-2e0, 5e0), 'inline components';
is Complex.new(-2, 5).conj, -2-5i, 'inline conjugate';
is Complex.new(0, 0).re, 0, 'zero real component';
is Complex.new(0, 0).im, 0, 'zero imaginary component';

# The owner is Complex; non-Complex numeric receivers keep their old path.
is 7.conj, 7, 'Int conjugate is itself';
ok Complex.^can('re').elems, 'Complex declares re';
ok Complex.^can('reals').elems, 'Complex declares reals';
