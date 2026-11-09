use Test;

plan 4;

my $s = 'abc';
my $s_result = $s ~~ S/b/X/;
is $s, 'abc', 'S/// under smartmatch leaves its LHS unchanged';
is $s_result, 'aXc', 'S/// under smartmatch returns the substituted copy';

my $w = 'banana';
my $w_result = $w ~~ TR/a/o/;
is $w, 'banana', 'TR/// under smartmatch leaves its LHS unchanged';
is $w_result, 'bonono', 'TR/// under smartmatch returns the translated copy';

done-testing;
