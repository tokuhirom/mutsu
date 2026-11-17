use Test;

# An object reblessed into a mixin type by `$obj does R` (#17890): a role
# attribute named like one of the class's own is a separate attribute -- the
# role's methods and the accessor read the role's copy, the class's methods
# their own -- and `.^roles` lists the mixed-in roles first, most recent
# first, without deduping a role the class already does. Checked against
# rakudo.

plan 7;

role HasX { has $.x = 'role-x'; method rx { $!x } }
class V { has $.x = 'class-x'; method vx { $!x } }
my $v = V.new;
$v does HasX;
is $v.x, 'role-x', 'the accessor reads the role attribute';
is $v.rx, 'role-x', 'so does a role method';
is $v.vx, 'class-x', "a class method still reads the class's own";
is V.new.x, 'class-x', 'other instances are untouched';

role R { has $!n = 1; method rn { $!n } }
class P { has $!n = 5; method pn { $!n } }
my $p = P.new;
$p does R;
is "{$p.rn}{$p.pn}", '15', 'the same for a private attribute';

role A { }
class C does A { }
my $o = C.new;
$o does A;
is $o.^roles.map(*.^name).join(','), 'A,A', '.^roles keeps a role the class also does';
my $q = C.new;
$q does HasX;
$q does A;
is $q.^roles.map(*.^name).join(','), 'A,HasX,A', 'mixed-in roles come first, most recent first';
