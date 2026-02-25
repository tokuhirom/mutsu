use Test;

plan 8;

my @matrix[2;2];
ok @matrix[0]:exists, 'shaped declaration creates first row';
@matrix[1;1] = 41;
is @matrix[1;1], 41, 'multidimensional shaped declaration indexes with ;';

my @fixed[1];
@fixed[0] = 7;
is @fixed[0], 7, 'single-dimension shaped declaration supports assignment';

my @seen;
{
    push @seen, 1;
}
for 1..2 -> $i {
    push @seen, $i + 1;
}
is-deeply @seen, [1, 2, 3], 'for-loop after block is parsed as a separate statement';

my int $x = 1;
is cas($x, 1, 9), 1, 'cas returns observed scalar value';
is $x, 9, 'cas updates scalar when expected matches';

my @vals;
@vals[0] = 3;
is cas(@vals[0], 3, 8), 3, 'cas returns observed indexed value';
is @vals[0], 8, 'cas updates indexed value when expected matches';
