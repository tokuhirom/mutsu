use Test;

# A package variable written inside a closure is the global package variable,
# not per-closure captured state: a later write from elsewhere must be seen by
# the closure's next call, and the closure's own later writes must reach
# everyone else. Found in UserTimezone, whose `user-timezone` sub (declared
# inside `sub EXPORT`) caches into `$UserTimezone::timezone` while an exported
# override sub writes the same variable.

plan 7;

my &g = do { sub g { my $r = $P::tz; $P::tz = 'd'; $r } };
is g(), Any, 'first call reads the unset package var';
is $P::tz, 'd', 'closure write reaches the package var';
$P::tz = 'X';
is g(), 'X', 'closure sees a write made outside it';
$P::tz = 'Y';
g();
is $P::tz, 'd', 'second closure write reaches the package var';

# The UserTimezone shape: a cached getter and a separate setter.
sub make-getter { sub get-tz { .return with $Q::tz; $Q::tz = 'default' } }
my &get-tz = make-getter();
sub set-tz($t) { $Q::tz = $t }
is get-tz(), 'default', 'getter caches its default';
set-tz('Asia/Tokyo');
is get-tz(), 'Asia/Tokyo', 'getter sees the override';

# Lexical captured state is still per-closure.
sub counter { my $n = 0; -> { ++$n } }
my &c1 = counter;
my &c2 = counter;
c1(); c1();
is c2(), 1, 'lexical captures stay per closure instance';
