use Test;

# Every routine and every compilation unit's mainline declares `$/`, `$!` and
# `$¢` in its outermost frame, so `MY::` of a sub, method or mainline lists
# them; a bare block, pointy block or loop body does not. The
# Rakudo-Type-Introspection distribution walks `MY::.pairs` and reaches `Nil`
# (and through it `Cool`) via the mainline's `$!` / `$/` (#17674).

plan 12;

sub specials(\stash) { stash.keys.grep({ $_ eq '$!' | '$/' | '$¢' }).sort.join(' ') }

is specials(MY::), '$! $/ $¢', 'the mainline declares $!, $/ and $¢';
is MY::<$/>.^name, 'Nil', 'the mainline $/ starts as Nil';
is MY::<$!>.^name, 'Nil', 'the mainline $! starts as Nil';

sub f { specials(MY::) }
is f(), '$! $/ $¢', 'a sub declares them';

my $anon = sub { specials(MY::) };
is $anon(), '$! $/ $¢', 'an anonymous sub declares them';

class C { method m { specials(MY::) } }
is C.m, '$! $/ $¢', 'a method declares them';

{ is specials(MY::), '', 'a bare block does not' }

my $pointy = -> { specials(MY::) };
is $pointy(), '', 'a pointy block does not';

for 1 { is specials(MY::), '', 'a loop body does not' }

sub g { my $p = -> { specials(MY::) }; $p() }
is g(), '', 'a pointy block inside a sub does not';

sub h { 'q' ~~ /q/; ~MY::<$/> }
is h(), 'q', "MY::<\$/> reads the routine's current match";

sub k { try die 'oops'; MY::<$!>.message }
is k(), 'oops', "MY::<\$!> reads the routine's current error";
