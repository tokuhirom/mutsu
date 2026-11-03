use Test;

# A `Str` that did a role (`"abc" but R`): `substr-rw` is a Str method on it,
# keeps the mixin when it splices, and a role method whose invocant is raw
# (`\SELF`) writes through the caller's variable, including the role's own
# `AT-POS` behind `$s[i] = v` (#16100). Checked against rakudo.

plan 12;

role StrIdx does Positional {
    method AT-POS(\SELF: $i) is raw { SELF.substr-rw($i, 1) }
}
role Plain {
    method put-at(\SELF: $i) is raw { SELF.substr-rw($i, 1) }
    method clobber(\SELF:) { SELF = "changed" }
}

my $s = "abc" but Plain;
is $s.substr-rw(1, 1), 'b', 'substr-rw on a but-mixed Str answers the window';

my $u = "abc" but Plain;
$u.substr-rw(1, 1) = "Q";
is $u, 'aQc', 'assigning through substr-rw splices the string';
ok $u ~~ Plain, '...and the value still does the role';

my $m = "abc" but Plain;
$m.clobber;
is $m, 'changed', 'a raw invocant parameter binds the variable itself';

my $v = "abc" but Plain;
$v.put-at(1) = "V";
is $v, 'aVc', 'a role method handing back substr-rw writes through';

my $w = "abc" but Plain;
my $r := $w.put-at(2);
is $r, 'c', 'a bound result reads the window';
$r = "U";
is $w, 'abU', '...and writes through to the variable';

my $x = "asd" but StrIdx;
is $x[1], 's', 'the role AT-POS answers a subscript read';
$x[1] = "X";
is $x, 'aXd', 'a subscript assignment goes through the role AT-POS';
ok $x ~~ StrIdx, '...and the value still does the role';

my $plain = "abc";
$plain.substr-rw(0, 1) = "Z";
is $plain, 'Zbc', 'an ordinary Str is unchanged behaviour';
my $y = "abc" but StrIdx;
$y[2] = "Z";
is $y, 'abZ', 'the last index';
