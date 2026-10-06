use v6;
use Test;

# `Complex.Rat(epsilon)` / `Complex.FatRat(epsilon)`: like the zero-argument
# forms, the number is a `Real` when its imaginary part is `≅ 0` under
# `$*TOLERANCE`, and the answer is the real part's own `.Rat(epsilon)` /
# `.FatRat(epsilon)`. The epsilon forms used to answer `0` for every Complex.
# `Num.FatRat(epsilon)` itself ignored its epsilon.

plan 23;

# --- Complex with a negligible imaginary part
is (3.7+0i).Rat(1e-2), 3.7, '(3.7+0i).Rat(1e-2)';
is (3.14159+0i).Rat(0.01), 3.142857, '(3.14159+0i).Rat(0.01) is the 22/7 approximation';
is (3.14159+0i).Rat(0.01).raku, '<22/7>', '... exactly 22/7';
is (3.14159+0i).FatRat(0.01), 3.142857, '(3.14159+0i).FatRat(0.01)';
is (3.14159+0i).FatRat(0.01).raku, 'FatRat.new(22, 7)', '... exactly 22/7';
isa-ok (3.14159+0i).Rat(0.01), Rat, '.Rat(eps) answers a Rat';
isa-ok (3.14159+0i).FatRat(0.01), FatRat, '.FatRat(eps) answers a FatRat';
is (3.14159+1e-20i).Rat(0.01).raku, '<22/7>', 'a tiny imaginary part is negligible';
is (-2.5+0i).Rat(0.1), -2.5, 'a negative real part';
is (-2.5+0i).FatRat(0.1).raku, 'FatRat.new(-5, 2)', '... as a FatRat too';
is (3.14159+0i).Rat(0).raku, '3.14159', 'a zero epsilon';
is (3.14159+0i).Rat.raku, (3.14159e0).Rat.raku, 'the zero-argument form is unchanged';

# --- the imaginary part decides
dies-ok { (3+2i).Rat(0.01) }, 'a non-negligible imaginary part dies for .Rat(eps)';
dies-ok { (3+2i).FatRat(0.01) }, '... and for .FatRat(eps)';
{
    my $*TOLERANCE = 5;
    is (3+2i).Rat(0.01), 3, 'a loose $*TOLERANCE makes it negligible';
    is (3+2i).FatRat(0.01).raku, 'FatRat.new(3, 1)', '... for .FatRat(eps) too';
}
{
    my $*TOLERANCE = 0;
    dies-ok { (3+0i).Rat(0.01) }, '$*TOLERANCE = 0 rejects even 3+0i';
}

# --- Num.FatRat(epsilon) honours its epsilon (it was ignored)
is 3.14159e0.FatRat(0.01).raku, 'FatRat.new(22, 7)', 'Num.FatRat(0.01) rationalizes with the epsilon';
is 3.7e0.FatRat(1e-2).raku, 'FatRat.new(37, 10)', 'Num.FatRat(1e-2)';
is 3.14159e0.FatRat(0).raku, 'FatRat.new(314159, 100000)', 'a zero epsilon is reduced';
is 3.14159e0.Rat(0.01).raku, '<22/7>', 'Num.Rat(0.01) is unchanged';
is 7.FatRat(0.5).raku, 'FatRat.new(7, 1)', 'Int.FatRat(eps) is unchanged';
is <7/3>.FatRat(1).raku, 'FatRat.new(7, 3)', 'Rat.FatRat(eps) is unchanged';
