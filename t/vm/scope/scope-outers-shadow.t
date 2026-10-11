use v6;
use Test;

# `$OUTERS::x` / `OUTERS::<$x>` start at the scope enclosing the running one,
# so a binding the running routine declares does not answer for its outer
# namesake (checked against Rakudo).
plan 5;

my $x = 5;
my &g = { $x };

sub plain { my $x = 6; my &k = { $x }; $OUTERS::x }
is plain(), 5, '$OUTERS::x reads past the routine\'s own binding';
sub stash { my $x = 6; my &k = { $x }; OUTERS::<$x> }
is stash(), 5, 'OUTERS::<$x> reads past the routine\'s own binding';
sub no-shadow { $OUTERS::x }
is no-shadow(), 5, '$OUTERS::x without a shadow';
sub in-block { my $x = 6; my &k = { $x }; my $r; if 1 { $r = $OUTERS::x }; $r }
is in-block(), 6, '$OUTERS::x from a nested block sees the routine\'s binding';
sub outer-r { my $x = 1; my &k = { $x }; sub inner-r { $OUTERS::x }; inner-r() }
is outer-r(), 1, '$OUTERS::x in a nested routine reads the enclosing routine\'s binding';
