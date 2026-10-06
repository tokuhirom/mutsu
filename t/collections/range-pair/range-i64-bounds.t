use Test;

# An Int range whose real bound is the largest/smallest Int. The compact Range
# kinds store an open end (`1..*`, `1..Inf`) as the sentinel i64::MAX (an open
# start as i64::MIN), so a genuine bound there used to read as an open range.
# A genuine bound at the sentinel is a GenericRange of two Ints instead.
# Every answer below was checked against Rakudo.

plan 7;

my $max = 9223372036854775807;
my $min = -9223372036854775808;

subtest 'the three lines of the issue', {
    plan 3;
    is (1..$max).infinite, False, 'a range ending at the largest Int is not infinite';
    is (1..$max).is-int, True, 'both endpoints are Ints';
    is ($min..5).is-int, True, 'a range starting at the smallest Int is is-int';
}

subtest 'open ranges stay open', {
    plan 5;
    is (1..*).infinite, True, '1..*';
    is (1..Inf).infinite, True, '1..Inf';
    is (-Inf..5).is-int, False, '-Inf..5 is not is-int';
    is (1..*).is-int, False, '1..* is not is-int';
    is (1..^*).infinite, True, '1..^*';
}

subtest 'every exclusive form at the bound', {
    plan 8;
    is (1..^$max).infinite, False, '1..^MAX';
    is (1..^$max).elems, $max - 1, '1..^MAX has MAX - 1 elements';
    is (1..^$max).max, $max, 'max is the excluded end, as Rakudo answers';
    is ($max^..$max).elems, 0, 'MAX^..MAX is empty';
    is ($max-1^..^$max).elems, 0, 'MAX-1^..^MAX is empty';
    is (^$max).infinite, False, '^MAX';
    is (^$max).elems, $max, '^MAX has MAX elements';
    is ($min^..$min+2).list.join(' '), "{$min + 1} {$min + 2}", 'MIN^..MIN+2';
}

subtest 'sizes and bounds', {
    plan 8;
    is ($max-2..$max).elems, 3, 'elems';
    is ($min..$max).elems, 18446744073709551616, 'the full Int range has 2**64 elements';
    is ($min..$max).infinite, False, 'the full Int range is not infinite';
    is ($min..$max).is-int, True, 'the full Int range is-int';
    is (1..$max).max, $max, 'max';
    is ($min..5).min, $min, 'min';
    is (1..$max).minmax.join(' '), "1 $max", 'minmax';
    ok (1..$max) eqv (1..$max), 'eqv';
}

subtest 'iteration reaches the bound and stops', {
    plan 6;
    is ($max-2..$max).list.join(' '), "{$max - 2} {$max - 1} $max", 'list';
    is ($max-1..$max).reverse.join(' '), "$max {$max - 1}", 'reverse';
    is ($min..$min+2).list.join(' '), "$min {$min + 1} {$min + 2}", 'list from the smallest Int';
    is ($max-2..^$max).list.join(' '), "{$max - 2} {$max - 1}", 'exclusive end';
    is ($max..$max).list.join(' '), "$max", 'a one-element range at the bound';
    is ($max..$max+1).list.elems, 2, 'a range that goes past the bound is bigger than an Int';
}

subtest 'lazy consumers do not walk to the bound', {
    plan 6;
    is (1..$max).head(3).join(' '), '1 2 3', 'head';
    is (1..$max).first(* > 5), 6, 'first';
    is (1..$max).iterator.pull-one, 1, 'pull-one';
    is (1..$max).map(* * 2).head(2).join(' '), '2 4', 'map';
    is (1..$max).grep(* %% 2).head(2).join(' '), '2 4', 'grep';
    is (1..$max)[^3].join(' '), '1 2 3', 'a positional slice';
}

subtest 'membership, sum and raku', {
    plan 6;
    ok $max ~~ (1..$max), 'the bound is in the range';
    ok $min ~~ ($min..5), 'the smallest Int is in the range';
    nok 0 ~~ (1..$max), 'below the start';
    is ([+] $max-2..$max), 27670116110564327418, 'sum overflows into a big Int';
    is ($max-1..$max).raku, "{$max - 1}..$max", 'raku';
    is (1..$max).WHAT.^name, 'Range', 'a Range';
}
