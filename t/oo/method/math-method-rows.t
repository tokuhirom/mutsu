use Test;

# ADR-11276 slice 3B: the transcendental math methods are handler rows. Rakudo
# declares each on Int, Num, Rat and Complex and on Cool (`self.Numeric.sin`),
# so one handler answers a number, a Complex and every Cool receiver (a Str, a
# List, an Array, a Hash). The expected values are Rakudo's.

plan 12;

sub approx-complex($got, $re, $im, $why) {
    ok $got ~~ Complex && abs($got.re - $re) < 1e-12 && abs($got.im - $im) < 1e-12,
        "$why: $got";
}

subtest 'real receivers answer a Num', {
    plan 11;
    for (7, 1.5, 1.5e0, 7.FatRat) -> $x {
        isa-ok $x.sin, Num, "{$x.^name}.sin is a Num";
    }
    is-approx 1.sin, 0.8414709848078965, 'Int.sin';
    is-approx 1.5.cos, 0.0707372016677029, 'Rat.cos';
    is-approx 2.5e0.tanh, 0.9866142981514303, 'Num.tanh';
    is-approx (10**30).sin, 0.009331468931175825, 'a big Int goes through f64';
    is-approx 1.FatRat.sec, 1.8508157176809255, 'FatRat.sec';
    is-approx 0.5.atanh, 0.5493061443340549, 'Rat.atanh';
    is-approx 2.acosh, 1.3169578969248166, 'Int.acosh';
}

subtest 'the inverse and reciprocal forms', {
    plan 12;
    is-approx 1.asec, 0, 'asec';
    is-approx 1.acosec, 1.5707963267948966, 'acosec';
    is-approx 1.acotan, 0.7853981633974483, 'acotan';
    is-approx 1.asech, 0, 'asech';
    is-approx 1.acosech, 0.881373587019543, 'acosech';
    is-approx 3.acotanh, 0.3465735902799726, 'acotanh';
    is-approx 1.cosec, 1.1883951057781212, 'cosec';
    is-approx 1.cotan, 0.6420926159343306, 'cotan';
    is-approx 1.sech, 0.6480542736638855, 'sech';
    is-approx 1.cosech, 0.8509181282393216, 'cosech';
    is-approx 1.cotanh, 1.3130352854993313, 'cotanh';
    ok 2.asech.isNaN, 'asech outside its domain is NaN';
}

subtest 'exp, log and sqrt', {
    plan 9;
    is-approx 1.exp, 2.718281828459045, 'exp';
    is-approx 10.log, 2.302585092994046, 'log';
    is-approx 8.log2, 3, 'log2';
    is-approx 1.5.log10, 0.17609125905568124, 'log10';
    is 4.sqrt, 2, 'sqrt of a perfect square';
    isa-ok 4.sqrt, Num, 'sqrt answers a Num';
    is (4/9).sqrt, 0.6666666666666666, 'sqrt of a Rat';
    ok (-4).sqrt.isNaN, 'sqrt of a negative real is NaN';
    is (10**40).sqrt, 1e20, 'sqrt of a big Int';
}

subtest 'Complex receivers take the complex formulas', {
    plan 8;
    approx-complex 2i.sin, 0, 3.626860407847019, 'sin';
    approx-complex <1+1i>.cos, 0.8337300251311491, -0.9888977057628651, 'cos';
    approx-complex <1+1i>.exp, 1.4686939399158851, 2.2873552871788423, 'exp';
    approx-complex <1+1i>.log, 0.3465735902799727, 0.7853981633974483, 'log';
    approx-complex <1+1i>.log2, 0.5000000000000001, 1.1330900354567985, 'log2';
    approx-complex <1+1i>.log10, 0.15051499783199063, 0.34109408846046035, 'log10';
    approx-complex 4i.sqrt, 1.4142135623730951, 1.4142135623730951, 'sqrt';
    approx-complex <1+1i>.acosh, 1.0612750619050357, 0.9045568943023814, 'acosh';
}

subtest 'Cool receivers numify first', {
    plan 8;
    is-approx "0.5".sin, 0.479425538604203, 'a Str numifies by parsing';
    is-approx "1/2".sin, 0.479425538604203, 'a Rat string';
    is-approx "0x10".cos, -0.9576594803233847, 'a radix prefix';
    is-approx [1, 2, 3].sin, 0.1411200080598672, 'an Array numifies to its element count';
    is-approx (1, 2, 3).sqrt, 1.7320508075688772, 'a List too';
    is-approx %(a => 1, b => 2).log2, 1, 'a Hash numifies to its pair count';
    approx-complex "1+1i".exp, 1.4686939399158851, 2.2873552871788423, 'a Complex string';
    isa-ok "1.5".sqrt, Num, 'the answer is a Num';
}

subtest 'a non-numeric Str is the X::Str::Numeric failure', {
    plan 3;
    throws-like { "abc".sin }, X::Str::Numeric, 'sin';
    throws-like { "abc".sqrt }, X::Str::Numeric, 'sqrt';
    throws-like { "abc".log(2) }, X::Str::Numeric, 'log with a base';
}

subtest 'atan2 takes an optional x', {
    plan 7;
    is-approx 3.atan2, 1.2490457723982544, 'Int.atan2 with the default x';
    is-approx 3.atan2(2), 0.982793723247329, 'Int.atan2(2)';
    is-approx 1.5.atan2(2.5), 0.5404195002705842, 'Rat.atan2(Rat)';
    is-approx "3".atan2, 1.2490457723982544, 'Cool.atan2 numifies a Str';
    is-approx "3".atan2("2"), 0.982793723247329, 'and its argument';
    is-approx [1, 2].atan2(2), 0.7853981633974483, 'a List numifies to its count';
    is-approx 1.atan2(-1), 2.356194490192345, 'a negative x';
}

subtest 'log and exp with a base', {
    plan 6;
    is-approx 8.log(2), 3, 'Int.log(Int)';
    is-approx "2".log("3"), 0.6309297535714574, 'a Str base';
    is-approx 2.exp(3), 9, 'x.exp(base) is base ** x';
    is-approx 1.5.exp(2), 2.8284271247461903, 'Rat.exp(Int)';
    approx-complex 1.log(1i), 0, 0, 'a Complex base';
    approx-complex <1+1i>.log(2), 0.5, 1.1330900354567985, 'a Complex receiver';
}

subtest 'cis, unpolar, polar and roots', {
    plan 8;
    approx-complex 5.cis, 0.28366218546322625, -0.9589242746631385, 'Int.cis';
    approx-complex "5".cis, 0.28366218546322625, -0.9589242746631385, 'Cool.cis';
    approx-complex <1+1i>.cis, 0.19876611034641298, 0.30955987565311222, 'Complex.cis';
    approx-complex 5.unpolar(0), 5, 0, 'Int.unpolar';
    is <3+4i>.polar.map(*.round(0.001)).List, (5, 0.927), 'Complex.polar';
    is 8.roots(3).elems, 3, 'Int.roots';
    approx-complex 4.roots(2)[0], 2, 0, 'the first root';
    is "1".roots(2).elems, 2, 'Cool.roots numifies a Str';
}

subtest 'expmod belongs to Int only', {
    plan 3;
    is 4.expmod(2, 5), 1, 'Int.expmod';
    is (10**20).expmod(3, 7), 1, 'a big Int';
    dies-ok { "4".expmod(2, 5) }, 'a Str has no expmod (it is declared on Int, not Cool)';
}

subtest 'the method table exposes the rows', {
    plan 6;
    ok Int.^can('sin') && Num.^can('sin') && Rat.^can('sin') && Complex.^can('sin') && Cool.^can('sin'),
        'every owner has sin';
    ok Cool.^can('atan2') && Int.^can('atan2') && Rat.^can('atan2'), 'atan2 is an Int, Rat and Cool method';
    ok Complex.^can('polar') && !Int.^can('polar') && !Cool.^can('polar'), 'polar is a Complex method only';
    ok Int.^can('expmod') && !Cool.^can('expmod'), 'expmod is an Int method only';
    ok Cool.^can('acosech') && Cool.^can('acotanh'), 'the inverse forms are Cool methods too';
    ok (^3).map({ 1.sin }).all ~~ Num, 'a repeated call site answers each time';
}

subtest 'Bool is an Int enum: Int and Cool rows answer it', {
    plan 9;
    is-approx True.sin, 0.8414709848078965, 'True.sin';
    is True.sqrt, 1, 'True.sqrt';
    is-approx False.exp, 1, 'False.exp';
    is-approx True.atan2(2), 0.4636476090008061, 'True.atan2(2)';
    approx-complex True.cis, 0.5403023058681398, 0.8414709848078965, 'True.cis';
    is True.abs, 1, 'True.abs is the Int 1';
    is False.sign, 0, 'False.sign';
    is-deeply True.floor, True, 'True.floor is True itself';
    is-deeply False.truncate, False, 'False.truncate is False itself';
}
