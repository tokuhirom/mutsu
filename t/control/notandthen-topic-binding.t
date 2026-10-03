use Test;

# `notandthen` binds its (undefined) left operand to `$_` while evaluating the
# right operand, like `orelse`. mutsu used to leave the outer topic in place.

plan 6;

is (Int notandthen $_.raku), 'Int', 'topic is the undefined LHS';
is (Str notandthen Int notandthen $_.raku), 'Int', 'chained: topic is the nearest LHS';
is (Int notandthen 5), 5, 'RHS value is the result';
is-deeply (3 notandthen $_), Empty, 'defined LHS skips the RHS';

$_ = 7;
my $r = (Int notandthen $_.^name);
is $r, 'Int', 'topic set inside the RHS';
is $_, 7, 'outer topic restored afterwards';
