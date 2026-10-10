use Test;

# From Distribution::Extension::Updater: `*%_ ()` rejects any named argument.

plan 4;

sub f(*%_ ()) { "ok" }
is f(), 'ok', 'no named arguments is accepted';
dies-ok { f(zzz => 1) }, 'a named argument is rejected by the empty unpack';

sub g(*%h (:$a)) { "a=$a" }
is g(a => 1), 'a=1', 'unpack binds the named leaf';
dies-ok { g(zzz => 1) }, 'unknown named argument is rejected';
