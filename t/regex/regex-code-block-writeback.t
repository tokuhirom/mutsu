use Test;

plan 7;

# A regex `{ … }` block writes to the enclosing scope's lexicals, and those
# writes must be visible after the match. mutsu detects them by comparing the
# block's env before and after by *binding identity*, so each of these shapes has
# to survive: a fresh value, a value that looks the same as the old one, and a
# container mutated in place (which keeps its binding).

my $n = 0;
ok 'abc' ~~ / a { $n = 42 } bc /, 'a match whose code block assigns';
is $n, 42, 'the assignment is visible afterwards';

# Rebinding to a value that stringifies the same as the old one still counts.
my $same = '1';
ok 'abc' ~~ / a { $same = 1 } bc /, 'a match that rebinds to a look-alike value';
is $same.WHAT.^name, 'Int', 'the new binding replaced the old one';

# A container mutated in place keeps its binding, and the mutation is shared.
my @acc;
ok 'abc' ~~ / a { @acc.push('seen') } bc /, 'a match whose code block mutates an array';
is-deeply @acc, ['seen'], 'the in-place mutation is visible afterwards';

# Several blocks in one match all land.
my @order;
'abc' ~~ / a { @order.push(1) } b { @order.push(2) } c /;
is-deeply @order, [1, 2], 'every block in the match ran and wrote back';
