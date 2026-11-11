use Test;

plan 9;

# `:delete` through a method that returns an attribute's aggregate deletes
# from the attribute itself, and an absent key answers the container default.

use nqp;
class O { has %.map; has @.arr; }
my $attr = O.^attributes.first(*.name eq '%!map');
O.^add_method('m1', my method () is rw { nqp::getattr(self, O, '%!map') });
O.^add_method('m2', my method () { $attr.get_value(self) });
O.^compose;

my $o = O.new(map => {old => 1, keep => 2, z => 3}, arr => [1, 2, 3]);
is $o.m1<old>:delete, 1, 'delete through an rw nqp::getattr method';
is-deeply $o.map, {keep => 2, z => 3}, 'the attribute lost the key';
is $o.m2<keep>:delete, 2, 'delete through a get_value method';
is-deeply $o.map, {z => 3}, 'the attribute lost that key too';
is $o.map<z>:delete, 3, 'delete through the public accessor';
is-deeply $o.map, {}, 'the attribute is empty';
ok ($o.map<nope>:delete) === Any, 'an absent key answers Any';
is $o.arr[2]:delete, 3, 'array element delete through the accessor';
is $o.arr.elems, 2, 'the trailing element is gone';
