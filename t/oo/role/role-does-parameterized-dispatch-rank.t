use v6;
use Test;

plan 3;

# An object a parameterized role was mixed into in place (`$o does R[...]`,
# reblessed since #17766) is narrower as that role than as its own class in a
# multi dispatch, as it is after `but R[...]`. The role is recorded under its
# application key (`R[Block]` plus the arguments' identity), which the
# ranking compared to the bare role name, so the role candidate lost to a
# wider one (XML::Class's `serialise($val, SerialiseX $a)`, t/110-custom).
role SX[&s] { method go($v) { &s($v) } }
class K {}

multi sub pick($val, SX $a) { "role" }
multi sub pick(Mu $val, K $a) { "class" }

my $k = K.new;
$k does SX[-> $v { $v }];
is pick(1, $k), 'role', 'a parameterized role mixed in with does';
is pick(1, K.new but SX[-> $v { $v }]), 'role', 'and with but';

multi sub ser($val, SX $a) { "serialise " ~ $a.go($val) }
multi sub ser(Mu $val, Attribute $a, $element?) { "generic" }
my $attr = Attribute.new(:name<$!x>, :type(Int), :package(Any));
$attr does SX[-> $v { "<$v>" }];
is ser(1, $attr), 'serialise <1>', 'an Attribute a trait mixed the role into';
