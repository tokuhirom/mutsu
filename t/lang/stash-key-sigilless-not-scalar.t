use Test;

# A stash subscript key is the symbol as spelled: a sigil-less key does not
# find the `$` variable of that name (#17846).

plan 6;

our $x = 5;
is-deeply OUR::<x>, Any, 'OUR::<x> is not $x';
is OUR::<$x>, 5, 'OUR::<$x> is $x';

package P { our $y = 1; our sub f { 'f' } }
is-deeply P::<y>, Any, 'P::<y> is not $y';
is P::<$y>, 1, 'P::<$y> is $y';
is P::<&f>(), 'f', 'a routine is found under its & key';

my $stash = OUR::;
is-deeply $stash<x>, Any, 'a stash held in a variable answers the same';
