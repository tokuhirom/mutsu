use Test;

plan 6;

my $sum = infix:<+> 1, 2;
is $sum, 3, 'categorical operator term accepts listop arguments';

is (infix:<+>), 0, 'categorical operator term calls with no arguments';
is infix:<+>(1, 2), 3, 'parenthesized operator call still works';

my $bracketed = infix:['+'] 1, 2;
is $bracketed, 3, 'bracketed adverb operator name accepts listop arguments';
is (infix:['+']), 0, 'bracketed operator name calls with no arguments';

sub infix:<join-words>($a, $b) { $a ~ $b }
my $joined = infix:<join-words> 'a', 'b';
is $joined, 'ab', 'user operator name accepts listop arguments';
