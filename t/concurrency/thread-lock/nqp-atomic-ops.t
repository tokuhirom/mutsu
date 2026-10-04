use Test;
use nqp;

# The atomic nqp:: ops (#11502) share the Raku-level `⚛` / `cas` primitive.
# Expected answers are Rakudo 2026.09's.

plan 22;

{
    my int $i = 0;
    is nqp::atomicinc_i($i), 0, 'atomicinc_i answers the old value';
    is $i, 1, 'atomicinc_i increments';
    is nqp::atomicdec_i($i), 1, 'atomicdec_i answers the old value';
    is $i, 0, 'atomicdec_i decrements';
    is nqp::atomicadd_i($i, 5), 0, 'atomicadd_i answers the old value';
    is $i, 5, 'atomicadd_i adds';
    is nqp::atomicload_i($i), 5, 'atomicload_i';
    is nqp::atomicstore_i($i, 7), 7, 'atomicstore_i answers the stored value';
    is nqp::cas_i($i, 7, 9), 7, 'cas_i answers the old value on a match';
    is $i, 9, 'cas_i swaps on a match';
    is nqp::cas_i($i, 7, 11), 9, 'cas_i answers the current value on a miss';
    is $i, 9, 'cas_i leaves the target alone on a miss';
}

{
    my $o = [1];
    my $s = $o;
    ok nqp::cas($s, $o, [2]) === $o, 'cas answers the old object on a match';
    is-deeply $s, [2], 'cas swaps on an identity match';
    nqp::cas($s, $o, [3]);
    is-deeply nqp::atomicload($s), [2], 'cas compares by identity; atomicload reads';
    my $t;
    nqp::atomicstore($t, 1);
    is $t, 1, 'atomicstore';
}

{
    my @b = 1, 2;
    is nqp::cas(@b[0], 1, 5), 1, 'cas on an array element';
    is-deeply @b, [5, 2], 'cas on an array element swaps';
}

{
    class A { has int $.x; method bump { nqp::atomicinc_i($!x) } }
    my $a = A.new(x => 1);
    is $a.bump, 1, 'atomicinc_i on a native attribute';
    class B { has $.y }
    my $b = B.new(y => 1);
    nqp::atomicbindattr(nqp::decont($b), B, '$!y', 42);
    is $b.y, 42, 'atomicbindattr';
}

{
    my int $c = 0;
    my &r = { $c };
    nqp::atomicadd_i($c, 4);
    is r(), 4, 'an atomic write is seen through a closure capture';
}

{
    my atomicint $cnt = 0;
    await (^4).map: { start { nqp::atomicinc_i($cnt) for ^250 } };
    nqp::barrierfull();
    is $cnt, 1000, 'atomicinc_i from four threads loses no increment';
}
