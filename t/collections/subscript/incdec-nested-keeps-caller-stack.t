use Test;

# Found via Time::Duration: `++@w[0][1]` / `@w[0][1]++` inside a sub used as a
# call argument popped the caller's pending operands off the VM stack.

plan 6;

sub pre(@w)  { ++@w[0][1]; @w }
sub post(@w) { @w[0][1]++; @w }
sub dec(@w)  { --@w[0][1]; @w }

sub f() { my $d = 'a'; ($d, pre([[1, 1], [2, 2]])) }
is-deeply f(), ('a', [[1, 2], [2, 2]]), 'nested prefix ++ in an argument position';

sub g() { my $d = 'a'; ($d, post([[1, 1], [2, 2]])) }
is-deeply g(), ('a', [[1, 2], [2, 2]]), 'nested postfix ++ in an argument position';

sub h() { my $d = 'a'; ($d, dec([[1, 1], [2, 2]])) }
is-deeply h(), ('a', [[1, 0], [2, 2]]), 'nested prefix -- in an argument position';

sub r(Str $d, @a) { "$d|{@a.elems}" }
sub k(Str $neg) { my $direction = $neg; r($direction, pre([[1, 1], [2, 2]])) }
is k('x'), 'x|2', 'caller scalar still bound as the first argument';

is ('a', 'b', 'c', pre([[1, 1], [2, 2]]).elems).join(','), 'a,b,c,2', 'several pending operands survive';

my @m = [[5, 5], [6, 6]];
my $v = ++@m[1][0];
is $v, 7, 'the expression value is still the new value';
