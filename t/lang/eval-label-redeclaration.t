use Test;

plan 2;

my $counter = 0;
throws-like { EVAL q[label1: ++$counter; label1: 42] }, X::Redeclaration,
    'duplicate labels in EVAL throw X::Redeclaration';
is $counter, 0, 'duplicate-label error happens before executing prior statements';
