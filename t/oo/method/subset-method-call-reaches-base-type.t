use Test;

# From the PDF distribution (t/indobj-name.t): `MyName.COERCE('Foo')` on a
# subset of a role.
plan 3;

class K { method hi { "hi" } }
my subset SK of K where *.defined;
is SK.hi, "hi", 'class base';

role R { method rr { "rr" } }
my subset SR of R where 1;
is SR.rr, "rr", 'role base';

role Q { multi method COERCE(Str:D $s) { $s but Q } }
my subset SQ of Q where 'Foo' | 'Bar';
is SQ.COERCE('Foo').^name, 'Str+{Q}', 'COERCE through the subset';
