use Test;

# `OUTER::` from a unit's own mainline is outside the unit, so it sees none of
# the mainline's lexicals (#17898). A statement modifier `given` opens no
# scope; a `for` modifier, a block or a routine does.

plan 8;

my $x = 1;
constant foo = 3;
is-deeply OUTER::<$x>, Nil, 'OUTER::<$x> from the mainline';
is-deeply $OUTER::x, Nil, '$OUTER::x from the mainline';
my $r = 42;
$r = OUTER::<$x> given 1;
is-deeply $r, Any, 'a given modifier opens no scope (Nil assigned)';
$r = 42;
$r = OUTER::<foo> given 1;
is-deeply $r, Any, 'nor for a constant';
is (OUTER::<$x> for 1).head, 1, 'a for modifier runs its statement as a block';
{ is OUTER::<$x>, 1, 'OUTER:: from a block is the mainline' }
sub f { OUTER::<$x> }
is f(), 1, 'OUTER:: from a routine is the mainline';
{ my $x = 2; is OUTER::<$x>, 1, 'OUTER:: skips the shadowing block scope' }
