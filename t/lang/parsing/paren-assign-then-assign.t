use Test;

# `($x = $y) = 5` assigns through the parenthesised assignment: the inner
# assignment yields the container of $x, which the outer one then writes.
# Pinned when the statement-level fallback for it was deleted as dead code
# (the expression grammar owns the construct).

plan 3;

my ($x, $y) = (1, 2);
($x = $y) = 5;
is "$x $y", "5 2", '($x = $y) = 5 leaves $y alone and sets $x';

my $a;
($a = 3) = 7;
is $a, 7, '($a = 3) = 7 ends with the outer value';

my @l;
(@l = 1, 2) = 4, 5;
is @l.join(','), '4,5', 'an array assignment can be reassigned the same way';
