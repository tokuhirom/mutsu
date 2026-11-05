use Test;
plan 7;
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
