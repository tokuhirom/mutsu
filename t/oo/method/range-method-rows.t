use Test;

# ADR-11276 slice 3C: Range's own methods (bounds, is-int, infinite,
# int-bounds, in-range, rand) are handler rows owned by Range. Every answer
# below was checked against Rakudo.

plan 7;

subtest 'bounds', {
    plan 6;
    is-deeply (1..3).bounds, (1, 3), 'closed Int range';
    is-deeply (1^..^5).bounds, (1, 5), 'excluded ends are still the written ends';
    is-deeply (1..*).bounds, (1, Inf), 'an open end is Inf';
    is-deeply (-Inf..2).bounds, (-Inf, 2), 'an open start is -Inf';
    is-deeply (1.5..3.5).bounds, (1.5, 3.5), 'fractional ends';
    is-deeply ('a'..'c').bounds, ('a', 'c'), 'string ends';
}

subtest 'is-int', {
    plan 11;
    ok (1..3).is-int, 'Int ends';
    ok (1^..^5).is-int, 'excluded Int ends';
    nok (1..*).is-int, 'an open end is not an Int';
    nok (*..5).is-int, 'an open start is not an Int';
    nok (1..Inf).is-int, 'Inf is not an Int';
    nok (-Inf..5).is-int, '-Inf is not an Int';
    ok (1..10**20).is-int, 'a big Int end is an Int';
    ok (True..5).is-int, 'a Bool end is an Int';
    nok (1..5.5).is-int, 'a Rat end is not';
    nok (1.5..3).is-int, 'a Rat start is not';
    nok ('a'..'c').is-int, 'string ends are not';
}

subtest 'infinite', {
    plan 5;
    nok (1..3).infinite, 'closed';
    ok (1..*).infinite, 'open end';
    ok (1..Inf).infinite, 'Inf end';
    ok (-Inf..1).infinite, '-Inf start';
    nok ('a'..'c').infinite, 'strings';
}

subtest 'int-bounds', {
    plan 9;
    is-deeply (1..3).int-bounds, (1, 3), 'closed Int range';
    is-deeply (1^..^5).int-bounds, (2, 4), 'excluded Int ends shift the bounds';
    is-deeply (1..^5.0).int-bounds, (1, 4), 'an excluded integral end shifts';
    is-deeply (1..^5.5).int-bounds, (1, 5), 'an excluded fractional end floors';
    is-deeply (1^..5.5).int-bounds, (2, 5), 'an excluded Int start shifts';
    for (1..*), (1..Inf), ('a'..'c'), (1.1..5) -> $range {
        my $message;
        try { $range.int-bounds; CATCH { default { $message = .message } } }
        is $message, 'Cannot determine integer bounds', "$range.raku() has no integer bounds";
    }
}

subtest 'in-range', {
    plan 9;
    ok (1..3).in-range(2), 'a value inside';
    ok (1..3).in-range(2, 'Foo'), 'a value inside, with a label';
    ok ('a'..'c').in-range('b'), 'a string range';
    ok (1..*).in-range(1000000), 'an open end';
    ok (1^..3).in-range(3), 'an excluded start still contains the end';
    for ((1..3), 5, 'X'), ((1^..3), 1, 'Value'), (('a'..'c'), 'z', 'Letter') -> ($range, $value, $what) {
        my $error;
        try { $range.in-range($value, $what); CATCH { default { $error = $_ } } }
        is $error.message, "$what out of range. Is: $value.raku(), should be in $range.raku()",
            "$what out of range: the message";
    }
    my $class;
    try { (1..3).in-range(5); CATCH { default { $class = .^name } } }
    is $class, 'X::OutOfRange', 'the exception class';
}

subtest 'rand', {
    plan 11;
    my @closed = (1..3).rand xx 200;
    ok @closed.all ~~ Num, 'a Num';
    ok @closed.all >= 1 && @closed.all <= 3, 'inside the range';
    ok @closed.unique.elems > 20, 'not constant';
    my @end = (1..^3).rand xx 200;
    ok @end.all < 3, 'an excluded end is never returned';
    my @start = (1^..3).rand xx 200;
    ok @start.all > 1, 'an excluded start is never returned';
    my @both = (1^..^3).rand xx 200;
    ok @both.all > 1 && @both.all < 3, 'both excluded ends are never returned';
    my $fractional = (1.5..2.5).rand;
    ok $fractional ~~ Num && 1.5 <= $fractional <= 2.5, 'fractional ends';
    my $failure = ('a'..'c').rand;
    is $failure.WHAT.^name, 'Failure', 'a non-numeric range is a Failure';
    $failure.so;
    isa-ok (1..3).rand, Num, 'one call';
    is (1..3).rand.WHAT.^name, 'Num', 'the type name';
    my @many = (0..1000).rand xx 50;
    ok @many.all <= 1000, 'a wide range';
}

subtest 'minmax shares the is-int rule', {
    plan 4;
    is-deeply (True..5).minmax, (1, 5), 'a Bool end numifies';
    is-deeply (False..^3).minmax, (0, 2), 'an excluded end of a Bool range';
    is-deeply (1^..^5).minmax, (2, 4), 'excluded Int ends';
    is-deeply (1..3).minmax, (1, 3), 'closed Int range';
}
