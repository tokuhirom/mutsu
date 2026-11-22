use Test;

plan 12;

# A statement prefix takes a statement, so a same-line statement modifier is
# part of the prefix's statement (`once say 1 if $c` is `once (say 1 if $c)`),
# as in rakudo (#18510).

my @seen;
for 0, 1, 1 { once @seen.push($_) if $_ }
is-deeply @seen, [], 'once runs its modified statement on the first entry only';

@seen = ();
for ^2 { once @seen.push($_) for 1, 2 }
is-deeply @seen, [1, 2], 'once with a loop modifier loops inside the phaser';

my $x = 5;
$x = quietly 3 if 0;
is-deeply $x, Empty, 'quietly: the assignment runs, the modifier is inside';

my $y = 5;
$y = once 3 if 0;
is-deeply $y, Empty, 'once: likewise';

my $z = 5;
$z = sink 3 if 0;
is-deeply $z, Any, 'sink: likewise';

my @a = 1;
@a = eager 2, 3 if 0;
is-deeply @a, [], 'eager: likewise';

sub l($c) { return lazy 1..3 if $c; 42 }
isnt l(0), 42, 'return lazy X if C returns whatever the condition gives';
sub e($c) { return eager 1..3 if $c; 42 }
is-deeply e(0), Empty, 'return eager X if C';

my @b = 1, 2;
is (hyper @b if 1).^name, 'HyperSeq', 'hyper over a modified statement';
is (race @b if 0).^name, 'RaceSeq', 'race over a modified statement';

my $count = 0;
quietly $count++ for 1, 2, 3;
is $count, 3, 'quietly with a loop modifier';

my $next-line = 0;
quietly $next-line = 1;
if False { $next-line = 2 }
is $next-line, 1, 'a modifier keyword on the next line is a statement of its own';
