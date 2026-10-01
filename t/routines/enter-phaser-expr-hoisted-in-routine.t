use Test;

# From the Timer distribution: `now - ENTER now` inside a routine body must
# evaluate the ENTER operand at block entry, so the elapsed time is positive.
plan 5;

sub timer(&callable) { &callable(), now - ENTER now }
my ($v, $t) = timer { sleep 0.05; 5 };
is $v, 5, 'value survives';
ok $t > 0.03, 'comma-list element: ENTER now runs before the callable';

sub elapsed { sleep 0.05; now - ENTER now }
ok elapsed() > 0.03, 'binary operand in a sub body';

my &c = { sleep 0.05; now - ENTER now };
ok c() > 0.03, 'binary operand in a closure body';

sub z { my $x = 3; $x + ENTER 4 }
is z(), 7, 'plain value ENTER operand';
