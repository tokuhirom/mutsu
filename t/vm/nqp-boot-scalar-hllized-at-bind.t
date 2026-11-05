use Test;
use nqp;

plan 11;

# Rakudo's binder hllizes every argument: a BOOTInt / BOOTNum / BOOTStr box
# bound to a Raku parameter is the Int / Num / Str it holds, and the BOOT type
# object is the HLL type object. Assignment and `nqp::` ops leave a box raw
# (#12881).
sub named(\v) { v.^name }
sub scalar($v) { $v.^name }
sub typed(Str $v) { $v.^name }

my $s := nqp::box_s("x", nqp::bootstr());
my $i := nqp::box_i(3, nqp::bootint());
my $n := nqp::box_n(1.5e0, nqp::bootnum());

is named($s), 'Str', 'a sigilless parameter sees a Str';
is scalar($s), 'Str', 'a scalar parameter sees a Str';
is scalar($i), 'Int', 'a BOOTInt is an Int';
is scalar($n), 'Num', 'a BOOTNum is a Num';
is named(nqp::bootstr()), 'Str', 'the BOOTStr type object is the Str type object';
is typed($s), 'Str', 'a Str-typed parameter accepts the box';
is (-> \v { v.^name })($s), 'Str', 'a pointy block binds the hllized value';
is scalar(nqp::atpos(nqp::list($s), 0)), 'Str', 'an element read from a raw list is hllized at the call';
is $s.^name, 'BOOTStr', 'the box itself stays raw outside a binder';
is nqp::isstr($s), 1, 'and nqp::isstr still sees the raw box';
is scalar(nqp::list(1,2)), 'List', 'a raw list argument is hllized by a scalar parameter too';
