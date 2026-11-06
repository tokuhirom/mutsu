use Test;

# A package stash publishes an `our` scalar as the variable's own container, so
# stores through the stash reach the variable. From CustomImporting's
# `from-import` (#16920).

plan 12;

module TI {
    our $v1 = 42;
    our $v2 = 1;
    our @a = 1, 2;
}

my $items = TI.WHO;

$items<$v1> = 50;
is $TI::v1, 50, 'assigning through a stash subscript writes the variable';

$items<$v1>++;
is $TI::v1, 51, 'postfix ++ steps the variable';

--$items<$v1>;
is $TI::v1, 50, 'prefix -- steps the variable';

$items<$v1> += 5;
is $TI::v1, 55, 'a compound assignment writes the variable';

my $c := $items<$v2>;
$c = 7;
is $TI::v2, 7, 'a variable bound to a stash element aliases the variable';

$items{'$v2'} = 9;
is $TI::v2, 9, 'the curly spelling writes the variable';

my $k = 'v2';
$items{'$' ~ $k} = 10;
is $TI::v2, 10, 'a computed key writes the variable';
is $items<$v2>, 10, 'reading it back through the stash';

$items<@a>.push(3);
is @TI::a.join(','), '1,2,3', 'a container symbol is the package array itself';

is TI.WHO.AT-KEY('$v1'), 55, 'Stash.AT-KEY answers the variable';
ok TI.WHO.EXISTS-KEY('$v1'), 'Stash.EXISTS-KEY sees it';

TI::<$v1> = 60;
is $TI::v1, 60, 'the direct package subscript still writes';
