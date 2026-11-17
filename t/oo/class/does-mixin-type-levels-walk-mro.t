use Test;

# An object reblessed into a mixin type by `$obj does R` (#17890): every
# `does` adds a level of its own, even for a role the class already composes;
# `.^mro(:roles)` lists a role at each level that composes it; `.WALK` visits
# the mixed-in roles' methods; and a mixin type composed from a role is not
# reused for a later, redeclared role of the same name. Checked against
# rakudo.

plan 9;

role R { method m { 1 } }
class C { }
my $o = C.new;
$o does R;
$o does R;
is $o.^name, 'C+{R}+{R}', 'a second `does` of the same role adds a level';

class D does R { }
my $d = D.new;
$d does R;
is $d.^name, 'D+{R}', 'a role the class already does still gets a level';
is $d.^mro(:roles).map(*.^name).join(','), 'D+{R},R,D,R,Any,Mu',
    '.^mro(:roles) lists the role at both levels';
is D.^mro(:roles).map(*.^name).join(','), 'D,R,Any,Mu', 'the class alone lists it once';

role RA { }
role RB does RA { }
class Nest does RB { }
my $n = Nest.new;
$n does RB;
is $n.^mro(:roles).map(*.^name).join(','), 'Nest+{RB},RB,RA,Nest,RB,RA,Any,Mu',
    'a role reached through another is listed once per level';

class WBase { method foo { 'WBase::foo' } }
class WSub is WBase { method foo { 'WSub::foo' } }
role Z { method zork { 'Z::zork' }; method foo { 'Z::foo' } }
my $w = WSub.new;
$w does Z;
is $w.WALK('zork')().Str, 'Z::zork', '.WALK finds the mixed-in role method';
is $w.WALK('foo')().Str, 'Z::foo WSub::foo WBase::foo',
    'and visits it ahead of the class chain';

my @got;
for 1, 2 -> $i {
    my role Fresh { method v { $i } }
    my $x = C.new;
    $x does Fresh;
    @got.push($x.v);
}
is-deeply @got, [1, 2], 'a redeclared role of the same name is composed afresh';
is C.new.^name, 'C', 'the class itself is untouched';
