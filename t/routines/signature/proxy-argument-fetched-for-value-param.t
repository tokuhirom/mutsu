use Test;

# A Proxy held in a variable and passed to a plain `$x` parameter is FETCHed
# when the parameter binds: the parameter is a Scalar holding the value, and a
# built-in the body hands it to sees that value (#18099). An `is rw`, `is raw`
# or sigilless parameter still binds the Proxy itself.

plan 9;

my $p := Proxy.new(FETCH => method () { -9 }, STORE => method ($) { });

sub f($x) { abs($x) }
is f($p), 9, 'a body that only calls a built-in on the parameter';
is f(-9), 9, '... called with a plain value in between';
is f($p), 9, '... and with the Proxy again';

sub typed(Int $x) { abs($x) }
is typed($p), 9, 'a typed parameter';

sub g($x) { $x.VAR.^name ~ ' ' ~ $x.WHAT.^name }
is g($p), 'Scalar Int', 'the parameter is a Scalar holding the FETCHed value';
sub plus($x) { $x + 1 }
is plus($p), -8, 'arithmetic on the parameter';

sub r($x is rw) { $x.VAR.^name }
is r($p), 'Proxy', 'an is rw parameter binds the Proxy';
sub w($x is raw) { $x.VAR.^name }
is w($p), 'Proxy', 'an is raw parameter binds the Proxy';
sub q(\x) { x.VAR.^name }
is q($p), 'Proxy', 'a sigilless parameter binds the Proxy';
