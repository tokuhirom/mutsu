use Test;
plan 9;
class Plain { method iterator { (1,2,3).iterator } }
my $p = Plain.new;
ok !($p ~~ Sequence), 'a plain class is not a Sequence';
ok !($p.^roles.map(*.^name).grep('Sequence')), 'and lists no Sequence role';
for ^3 { $p.elems }  # repeated method calls ask the role question each time
ok !($p ~~ Sequence), 'still not a Sequence after repeated calls';
class Late does Sequence { method iterator { (4,5,6).iterator } }
my $l = Late.new;
is $l.list.join(','), '4,5,6', 'a class declared later that does Sequence delegates to its iterator';
is $l.list.join(','), '4,5,6', 'and again';
ok $l ~~ Sequence, 'it is a Sequence';
is Plain.new.list.elems, 1, 'the earlier plain class is unaffected';

# A native-class receiver asks the same role questions on every method call;
# the repeated answers stay the same.
use NativeCall;
my $c = CArray[num64].new;
$c[0] = 1.5e0;
$c[1] = 2.5e0;
is (^4).map({ $c.AT-POS($_ % 2) }).join(','), '1.5,2.5,1.5,2.5', 'repeated AT-POS calls on a CArray';
is (^3).map({ $c.elems }).join(','), '2,2,2', 'and repeated elems calls';
