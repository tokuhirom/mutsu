use Test;
use nqp;

plan 9;

# nqp::rebless (#11499): give an object a mixin type, in place.
class A { has $.x = 7 }
role R { method hi { "hi" ~ self.x } }
role S { method yo { "yo" } }

my $mt = A.^mixin(R, S);
my $m = A.new;
my $alias = $m;
my $r := nqp::rebless($m, $mt);
is $m.hi, 'hi7', 'the object has the mixin type\'s first role';
is $m.yo, 'yo', 'and its second';
is $alias.hi, 'hi7', 'an alias sees the new type';
is $r.hi, 'hi7', 'the op answers the object';
ok $m ~~ R && $m ~~ S, 'the object does the roles';
is $m.x, 7, 'its attributes are kept';

my $n = A.new;
try nqp::rebless($n, Int);
is $!.message, 'New type Int for A is not a mixin type', 'a type that is not a mixin type is refused';
$! = Nil;
try nqp::rebless($n, A);
nok $!, 'reblessing to the object\'s own type is accepted';
nok $n.can('hi'), 'and changes nothing';
