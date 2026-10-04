use Test;

# ADR-11276 slice 3: `Int`, `Num` and `Bool` on the numeric types are rows
# owned by `Int`, `Num`, `Rat` and `FatRat` (`Complex` only has `Bool`: its
# `Int`/`Num` read `$*TOLERANCE`). Calls on a variable are answered by the
# call-site lane; the loops run each site repeatedly.

plan 9;

subtest 'Int', {
    plan 6;
    my $n = -2.7e0;
    is-deeply $n.Int, -2, 'a Num truncates towards zero';
    is-deeply (-7/2).Int, -3, 'a Rat truncates towards zero';
    is-deeply FatRat.new(9, 2).Int, 4, 'a FatRat truncates';
    is-deeply ((2**70 + 1) / 2).Int, 2**69, 'a big rational truncates to a big Int';
    is-deeply (2**70).Int, 2**70, 'a big Int is itself';
    is-deeply (^3).map({ $n.Int }).List, (-2, -2, -2), 'one site, repeated';
}

is-deeply 1e30.Int, 1000000000000000019884624838656,
    'a Num past a machine word truncates to a big Int';

subtest 'Int failures', {
    plan 4;
    ok NaN.Int ~~ Failure, 'NaN.Int is a Failure';
    is NaN.Int.exception.^name, 'X::Numeric::CannotConvert', '... of X::Numeric::CannotConvert';
    ok (-Inf).Int ~~ Failure, '-Inf.Int is a Failure';
    ok (3/0).Int ~~ Failure, 'a zero-denominator Rat is a Failure';
}

subtest 'Num', {
    plan 6;
    my $r = 7/2;
    is-deeply $r.Num, 3.5e0, 'a Rat';
    is-deeply 5.Num, 5e0, 'an Int';
    is-deeply (2**70).Num, 1180591620717411303424e0, 'a big Int';
    is-deeply (3/0).Num, Inf, 'a positive zero-denominator Rat';
    ok (0/0).Num.isNaN, '0/0 is NaN';
    is-deeply (^3).map({ $r.Num }).List, (3.5e0, 3.5e0, 3.5e0), 'one site, repeated';
}

subtest 'Bool', {
    plan 6;
    is-deeply 0.Bool, False, 'Int zero';
    is-deeply (-0e0).Bool, False, 'Num negative zero';
    is-deeply NaN.Bool, True, 'NaN is true';
    is-deeply (0/5).Bool, False, 'Rat zero';
    is-deeply (0+0i).Bool, False, 'Complex zero';
    is-deeply (0+1i).Bool, True, 'a non-zero Complex';
}

is-deeply (3+0i).Int, 3, 'Complex.Int with a zero imaginary part';
throws-like { (1+2i).Int }, X::Numeric::Real, 'Complex.Int with an imaginary part';

subtest 'Str numifies before truncating', {
    plan 3;
    is-deeply "12".Int, 12, 'an integer string';
    is-deeply "3/4".Int, 0, 'a rational string';
    is-deeply "1e30".Int, 1000000000000000019884624838656, 'a big Num string';
}

ok Int.^can('Num') && Rat.^can('Int') && Complex.^can('Bool'), 'introspection sees the methods';
