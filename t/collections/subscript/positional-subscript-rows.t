use Test;

# ADR-11276 slice 3C: AT-POS and EXISTS-POS of the list-likes are handler
# rows (List, Array and Range own AT-POS; List and Range own EXISTS-POS).
# Every answer below was checked against Rakudo.

plan 4;

my @array = 10, 20, 30;
my $list = (10, 20, 30);

subtest 'AT-POS', {
    plan 9;
    is @array.AT-POS(0), 10, 'Array.AT-POS(0)';
    is @array.AT-POS(2), 30, 'Array.AT-POS(2)';
    is @array.AT-POS("1"), 20, 'Array.AT-POS of a Str index';
    is @array.AT-POS(1.9), 20, 'Array.AT-POS of a fractional index';
    is $list.AT-POS(0), 10, 'List.AT-POS(0)';
    is $list.AT-POS(2), 30, 'List.AT-POS(2)';
    is $list.AT-POS("1"), 20, 'List.AT-POS of a Str index';
    is $list.AT-POS(1.9), 20, 'List.AT-POS of a fractional index';
    is $list.AT-POS(5).raku, 'Nil', 'List.AT-POS past the end';
}

subtest 'EXISTS-POS', {
    plan 6;
    ok @array.EXISTS-POS(0), 'Array.EXISTS-POS(0)';
    ok @array.EXISTS-POS(2), 'Array.EXISTS-POS(2)';
    nok @array.EXISTS-POS(3), 'Array.EXISTS-POS(3)';
    ok $list.EXISTS-POS(0), 'List.EXISTS-POS(0)';
    ok $list.EXISTS-POS(2), 'List.EXISTS-POS(2)';
    nok $list.EXISTS-POS(3), 'List.EXISTS-POS(3)';
}

subtest 'a hole is absent', {
    plan 2;
    my @holey = 1, 2, 3;
    @holey[1]:delete;
    nok @holey.EXISTS-POS(1), 'the deleted element does not exist';
    ok @holey.EXISTS-POS(2), 'the next one does';
}

subtest 'receivers without a positional protocol', {
    plan 3;
    is 5.AT-POS(0), 5, 'Any.AT-POS(0) is the value';
    ok 5.EXISTS-POS(0), 'Any.EXISTS-POS(0)';
    nok 5.EXISTS-POS(1), 'Any.EXISTS-POS(1)';
}
