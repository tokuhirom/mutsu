use Test;

# #18228: `once(...)` -- the identifier directly followed by `(` -- calls a
# routine named `once`; only `once` followed by a block or a space-separated
# statement is the phaser.

plan 7;

sub once($x = 42) { $x }
is once(), 42, 'once() calls the user sub';
is once(7), 7, 'once(7) passes its argument';

my $branch;
with once(0) { $branch = 'with' } else { $branch = 'else' }
is $branch, 'with', 'the call is evaluated as a with condition';

my @seen;
for ^3 { @seen.push(once(5)) }
is-deeply @seen, [5, 5, 5], 'a call inside a loop runs every time';

my $runs = 0;
for ^3 { once { $runs++ } }
is $runs, 1, 'once followed by a block is still the phaser';

my @once-values;
for ^3 { @once-values.push: once 9 }
is-deeply @once-values, [9, 9, 9], 'once followed by a space and a statement is the phaser (its value is kept)';

my $called = 0;
sub once-tracker() { $called++ }
once-tracker() for ^2;
is $called, 2, 'a sub whose name starts with once is an ordinary call';
