use Test;
plan 5;

my $x = 0;
my $g = gather {
    $x += 1;
    take $x;
};

is $x, 0, 'gather is lazy';

my $sum = 0;
for @$g -> $v { $sum += $v; }

is $x, 1, 'gather runs on iteration';
is $sum, 1, 'gather yields value';

my $sum2 = 0;
for @$g -> $v { $sum2 += $v; }

is $x, 1, 'gather cache reuses results';
is elems($g), 1, 'elems forces lazy list';
