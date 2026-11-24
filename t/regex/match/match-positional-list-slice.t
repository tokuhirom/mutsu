use Test;

# A list slice of a Match's positional captures reads the positional
# captures (#19154); an associative slice still reads the named ones.

plan 6;

my $m = "ab12" ~~ / (<[a..z]>+) (\d+) /;
is $m[0, 1].map(~*).join(','), 'ab,12', '$m[0, 1]';
is $m[1, 0].map(~*).join(','), '12,ab', '$m[1, 0]';
is $m[1, 0, 5].map(*.defined).join(','), 'True,True,False', 'past the end is Nil';
ok $m[0, 1][0] ~~ Match, 'the slice holds the capture Match objects';
my @i = 1, 0;
is $m[@i].map(~*).join(','), '12,ab', 'array-valued index';

my $n = "ab" ~~ / $<x>=(a) $<y>=(b) /;
is $n{<x y>}.map(~*).join(','), 'a,b', 'named-capture slice unchanged';
