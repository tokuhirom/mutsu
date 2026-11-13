use Test;

# `Routine.is-wrapped` is True while a wrapper is in place (Red's
# compiled-callable guard reads it, t/124).

plan 6;

sub f { 1 }
nok &f.is-wrapped, 'a routine starts unwrapped';
my $h = &f.wrap({ callsame() + 1 });
ok &f.is-wrapped, 'wrapping it wraps it';
&f.unwrap($h);
nok &f.is-wrapped, 'unwrapping it unwraps it';

class C { method m { 1 } }
my $m = C.^lookup('m');
nok $m.is-wrapped, 'a method starts unwrapped';
my $mh = $m.wrap(method { callsame() + 1 });
ok $m.is-wrapped, 'a wrapped method is wrapped';
$m.unwrap($mh);
nok $m.is-wrapped, 'and unwrapped again';
