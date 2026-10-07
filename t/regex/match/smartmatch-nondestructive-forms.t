use Test;

plan 4;

my $s = 'abc';
my $s_result = $s ~~ S/b/X/;
is $s, 'abc', 'S/// under smartmatch leaves its LHS unchanged';
is $s_result, False, 'S/// under smartmatch returns False';

my $w = 'banana';
my $w_result = $w ~~ TR/a/o/;
is $w, 'banana', 'TR/// under smartmatch leaves its LHS unchanged';
is $w_result, False, 'TR/// under smartmatch returns False';

done-testing;
