use Test;

# ADR-11276 slice 3C: Range's own element methods (elems, min, max, minmax,
# Numeric, list, Array, reverse, sum) are handler rows owned by Range. Every
# answer below was checked against Rakudo.

plan 6;

subtest 'elems and Numeric', {
    plan 9;
    is (1..5).elems, 5, 'closed';
    is (1^..5).elems, 4, 'excluded start';
    is (1..^5).elems, 4, 'excluded end';
    is (1^..^5).elems, 3, 'both excluded';
    is (1.5..4.5).elems, 4, 'Rat ends';
    is ('a'..'e').elems, 5, 'string ends';
    is (5..1).elems, 0, 'an empty range';
    is (1..5).Numeric, 5, 'Numeric is the element count';
    is (1^..^5).Int, 3, 'Int too';
}

subtest 'open ranges are lazy', {
    plan 7;
    isa-ok (1..*).elems, Failure, 'elems of an open end is a Failure';
    isa-ok (-Inf..3).elems, Failure, 'elems of an open start is a Failure';
    is (1..*).Numeric, Inf, 'Numeric is Inf';
    is (-Inf..3).Numeric, Inf, 'Numeric of an open start is Inf too';
    my $error;
    try { (1..*).Int; CATCH { default { $error = .message } } }
    is $error, 'Cannot convert Inf to Int', 'Int of an open range dies';
    ok (1..*).list.is-lazy, 'list stays lazy';
    ok (1..*).Array.is-lazy, 'Array stays lazy';
}

subtest 'min, max and minmax', {
    plan 12;
    is (1..5).min, 1, 'min';
    is (1^..5).min, 1, 'min keeps an excluded start as written';
    is (1..^5).max, 5, 'max keeps an excluded end as written';
    is (1.5..4.5).min, 1.5, 'min of Rat ends';
    is ('a'..'e').max, 'e', 'max of string ends';
    is (1..*).max, Inf, 'an open end is Inf';
    is (-Inf..3).min, -Inf, 'an open start is -Inf';
    is-deeply (1..5).minmax, (1, 5), 'minmax';
    is-deeply (1^..^5).minmax, (2, 4), 'minmax folds excluded Int ends';
    is-deeply (1.5..4.5).minmax, (1.5, 4.5), 'minmax of Rat ends';
    is-deeply (1..*).minmax, (1, Inf), 'minmax of an open end';
    is-deeply (5..1).minmax, (5, 1), 'minmax of an empty range';
}

subtest 'list and Array', {
    plan 8;
    is (1..5).list.raku, '(1, 2, 3, 4, 5)', 'list';
    is (1^..5).list.raku, '(2, 3, 4, 5)', 'list of an excluded start';
    is (1..^5).Array.raku, '[1, 2, 3, 4]', 'Array';
    is (1.5..4.5).list.raku, '(1.5, 2.5, 3.5, 4.5)', 'list of Rat ends';
    is ('a'..'c').list.raku, '("a", "b", "c")', 'list of string ends';
    is (5..1).list.raku, '()', 'list of an empty range';
    is (5..1).Array.raku, '[]', 'Array of an empty range';
    is (1..3).Array.elems, 3, 'a real Array';
}

subtest 'reverse and sum', {
    plan 6;
    is (1..5).reverse.raku, '(5, 4, 3, 2, 1).Seq', 'reverse';
    is (1^..^5).reverse.raku, '(4, 3, 2).Seq', 'reverse of excluded ends';
    is (5..1).reverse.raku, '().Seq', 'reverse of an empty range';
    is (1..5).sum, 15, 'sum';
    is (1^..5).sum, 14, 'sum of an excluded start';
    is (1..1).sum, 1, 'sum of one element';
}

subtest 'positional subscript on a Range', {
    plan 7;
    is (5..9).AT-POS(0), 5, 'AT-POS';
    is (5..9).AT-POS(4), 9, 'AT-POS of the last one';
    is (5..9).AT-POS(9).raku, 'Nil', 'AT-POS past the end is Nil';
    is (1..*).AT-POS(5), 6, 'AT-POS of an open range';
    ok (5..9).EXISTS-POS(4), 'EXISTS-POS';
    nok (5..9).EXISTS-POS(5), 'EXISTS-POS past the end';
    my $error;
    try { (1..*).EXISTS-POS(2); CATCH { default { $error = .^name } } }
    is $error, 'X::Cannot::Lazy', 'EXISTS-POS of an open range is lazy';
}
