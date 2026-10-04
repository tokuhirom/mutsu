use Test;

# ADR-11276 slice 3: `abs`, `sign`, `floor`, `ceiling`, `round` and
# `truncate` are rows owned by `Int`, `Num`, `Rat`, `FatRat` and `Complex`
# (no `Complex.sign`: Rakudo's comes from `Cool`). Calls on a variable are
# answered by the call-site lane; the loops below run each site repeatedly.

plan 12;

subtest 'Int', {
    plan 6;
    my $i = -7;
    is-deeply $i.abs, 7, 'abs';
    is-deeply $i.sign, -1, 'sign';
    is-deeply $i.floor, -7, 'floor';
    is-deeply $i.ceiling, -7, 'ceiling';
    is-deeply $i.round, -7, 'round';
    is-deeply (-2**70).abs, 2**70, 'abs of a big Int';
}

subtest 'Num', {
    plan 8;
    my $n = -2.5e0;
    is-deeply $n.abs, 2.5e0, 'abs';
    is-deeply $n.sign, -1, 'sign';
    is-deeply $n.floor, -3, 'floor';
    is-deeply $n.ceiling, -2, 'ceiling';
    is-deeply $n.round, -2, 'round half up';
    is-deeply $n.truncate, -2, 'truncate';
    is-deeply NaN.round.isNaN, True, 'NaN rounds to NaN';
    is-deeply (-Inf).floor, -Inf, '-Inf floors to -Inf';
}

subtest 'a Num past a machine word rounds to a big Int', {
    plan 4;
    is-deeply 1e30.floor, 1000000000000000019884624838656, 'floor';
    is-deeply (-1e30).ceiling, -1000000000000000019884624838656, 'ceiling';
    is-deeply 1e30.round, 1000000000000000019884624838656, 'round';
    is-deeply 1e30.truncate, 1000000000000000019884624838656, 'truncate';
}

subtest 'Rat', {
    plan 7;
    my $r = -7/2;
    is-deeply $r.abs, 7/2, 'abs keeps the Rat';
    is-deeply $r.sign, -1, 'sign';
    is-deeply $r.floor, -4, 'floor';
    is-deeply $r.ceiling, -3, 'ceiling';
    is-deeply $r.round, -3, 'round half up';
    is-deeply $r.truncate, -3, 'truncate';
    is-deeply (5/2).round, 3, 'round half up, positive';
}

is-deeply (9007199254740993/2).round, 4503599627370497,
    'rounding a word-sized Rat is exact past 2**53';

subtest 'FatRat', {
    plan 4;
    my $f = FatRat.new(-9, 4);
    is-deeply $f.abs, FatRat.new(9, 4), 'abs keeps the FatRat';
    is-deeply $f.sign, -1, 'sign';
    is-deeply $f.floor, -3, 'floor';
    is-deeply $f.round, -2, 'round';
}

subtest 'a big rational', {
    plan 3;
    my $b = -(2**70 + 1) / 2;
    is-deeply $b.floor, -(2**69) - 1, 'floor';
    is-deeply $b.round, -(2**69), 'round half up';
    is-deeply $b.abs, (2**70 + 1) / 2, 'abs';
}

subtest 'Complex', {
    plan 4;
    my $c = -1.5-2.5i;
    is-deeply (3+4i).abs, 5e0, 'abs is the magnitude';
    is-deeply $c.floor, -2-3i, 'floor of both parts';
    is-deeply $c.round, -1-2i, 'round of both parts';
    is-deeply $c.truncate, -1-2i, 'truncate of both parts';
}

throws-like { (1+2i).sign }, X::Numeric::Real, 'Complex.sign needs a Real';

is-deeply (-9223372036854775808/3).abs.raku, '<9223372036854775808/3>',
    'abs of an i64::MIN numerator stays a Rat';

subtest 'the call-site lane answers every iteration alike', {
    plan 2;
    my @got;
    for (-5/2, 5/2, -5/2) -> $r { @got.push: $r.round }
    is-deeply @got.List, (-2, 3, -2), 'one site, changing receivers';
    my $i = -3;
    is-deeply (^3).map({ $i.abs }).List, (3, 3, 3), 'one site, one receiver';
}

ok (3/0).floor ~~ Failure, 'a zero denominator answers a Failure';
