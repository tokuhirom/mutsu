use Test;

# A sigilless role parameter (`\t`) is bound under its term key; a role
# method naming it by its spelling (`t`) must still see the binding, however
# the role reaches the object: mixed into a class instance, into a value, in
# place with `does`, or composed into a class (#18058).

plan 7;

class Box { }
role R [Mu:U \t] { method foo { t } }
role R3 [::T] { method foo { T } }
role R2 [\t, $t] { method both { t.^name ~ ' ' ~ $t } }

is (Box.new but R[Int]).foo.^name, 'Int', 'mixed into a class instance';
is (1 but R[Int]).foo.^name, 'Int', 'mixed into a value';
is (Box.new but R3[Int]).foo.^name, 'Int', 'a ::T capture, for comparison';
my $b = Box.new;
$b does R[Str];
is $b.foo.^name, 'Str', 'mixed in place with does';
class K does R[Num] { }
is K.new.foo.^name, 'Num', 'composed into a class';
class K2 does R2[Rat, 'x'] { }
is K2.new.both, 'Rat x', 'a sigilless and a same-named scalar parameter stay apart';
is (Box.new but R2[Int, 'y']).both, 'Int y', 'likewise on a mixin';
