use Test;

# `Attribute` metamethods (`get_value`, `set_value`, `name`, ...) are answered
# from Attribute's own rows, for a plain attribute, one that `does` a role and
# an instance of a user subclass; a method the role or subclass declares
# itself still wins. From AttrX::Mooish, which calls `get_value` on a
# role-mixed attribute several times per attribute access.
plan 16;

role Tag { method tag { 'tagged' } }
role Over { method get_value($) { 'role-override' } }
my class C { has $.v; has @.l; has $!p = 'priv' }

my $c = C.new(v => 3, l => [1, 2]);
my ($v, $l, $p) = C.^attributes;

is $v.get_value($c), 3, 'get_value of a public scalar attribute';
is-deeply $l.get_value($c).List, (1, 2), 'get_value of an array attribute';
is $p.get_value($c), 'priv', 'get_value of a private attribute';
is $v.name, '$!v', 'name';

$v.set_value($c, 7);
is $c.v, 7, 'set_value writes the attribute';

my class M { has $.v }
my $m = M.new(v => 7);
my $mixed = M.^attributes[0];
$mixed does Tag;
is $mixed.get_value($m), 7, 'get_value on an attribute that does a role';
is $mixed.tag, 'tagged', 'the role method is there';
is $mixed.name, '$!v', 'name on the mixed attribute';
$mixed.set_value($m, 9);
is $m.v, 9, 'set_value on the mixed attribute';

my class O { has $.v }
my $over = O.^attributes[0];
$over does Over;
is $over.get_value(O.new(v => 1)), 'role-override', 'a role method of the same name wins';

my class MyAttr is Attribute {
    method get_value($obj) { 'subclass:' ~ callsame }
}
my class D { has $.w }
my $d = D.new(w => 5);
my $sub = MyAttr.new(name => '$!w', type => Any, package => D);
is $sub.name, '$!w', 'a user subclass instance answers name';
is $sub.get_value($d), 'subclass:5', 'a subclass method of the same name wins';

my $sum = 0;
for ^50 { $sum += $v.get_value($c) }
is $sum, 350, 'repeated get_value';

throws-like { $v.get_value(C) }, Exception,
    message => /'type object'/, 'get_value on a type object dies';

class E {
    has $.x is rw;
    method bump { my $a = self.^attributes[0]; $a.set_value(self, $a.get_value(self) + 1) }
}
my $e = E.new(x => 1);
$e.bump for ^3;
is $e.x, 4, 'get_value/set_value from inside a method';
is E.^attributes[0].get_value($e), 4, 'and read back';
