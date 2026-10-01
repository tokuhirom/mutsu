# Came from the `from` distribution (t/01-basic.rakutest): `MY::<&a &b>:p`
# slices several keys, and a symbol absent from MY:: reads as Nil.
use Test;

plan 8;

my $x = 5;
my $y = 6;
sub foo { 1 }

is-deeply MY::<$x $y>.List, (5, 6), 'MY::<$x $y> is a two-key slice';
is MY::<$x $y>:p.elems, 2, 'MY::<$x $y>:p yields both pairs';
is-deeply MY::<$x $y>:p.map(*.key).List, ('$x', '$y'), ':p keys keep the sigils';
is MY::<$x &foo>:p.elems, 2, 'mixed sigil slice with :p';
is MY::<&nope>:p.elems, 0, 'missing single key with :p is empty';
is-deeply MY::<&nope>, Nil, 'a symbol absent from MY:: is Nil';
is-deeply MY::<$nope>, Nil, 'a missing scalar is Nil too';
is-deeply "use Test; MY::<&plan &pass>:p".EVAL.map(*.key).sort.List, ('&pass', '&plan'),
    'slice of imported routines inside EVAL';
