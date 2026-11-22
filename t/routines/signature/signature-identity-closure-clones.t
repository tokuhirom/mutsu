use Test;
use nqp;

# #18728: every closure of one declaration answers the same `.signature`
# object, also when a parameter's `where` reads a captured variable, as
# rakudo's do; a signature literal is one object per declaration too.

plan 5;

sub mk($l) { sub ($x where * > $l) { } }
is nqp::objectid(mk(1).signature), nqp::objectid(mk(5).signature),
    'closures of one declaration share their signature';

sub mk-body($l) { sub ($x where * > $l) { $x } }
is nqp::objectid(mk-body(1).signature), nqp::objectid(mk-body(5).signature),
    'with a body too';

my @s;
for 1..2 -> $l { @s.push: :($x where * > $l) }
is nqp::objectid(@s[0]), nqp::objectid(@s[1]), 'a signature literal is one object';

# The shared signature binds in the scope of the closure it was read from.
my &one = mk(1);
ok \(3) ~~ &one.signature, 'reads the scope of the closure asked';
my &ten = mk(10);
nok \(3) ~~ &ten.signature, '...and of the next one';
