use Test;

# A role type object's `.^methods` / `.^attributes` report what the role
# itself declares; a `does` / `also does` parent keeps its own members, and a
# run-time mixin's `.^attributes` lists the mixed-in roles' attributes (#14142).

plan 12;

role A { has Int $.n is rw }
role B { also does A; method content { 1 } }
role C does A { method content { 1 } }

is B.^methods.map(*.name).join(','), 'content', 'also does: own methods only';
is B.^attributes.elems, 0, 'also does: parent attributes are not the role\'s own';
is B.^roles[0].^attributes.map(*.name).join(','), '$!n', 'also does parent lists its attribute';
is C.^methods.map(*.name).join(','), 'content', 'does: own methods only';
is C.^roles[0].^attributes.map(*.name).join(','), '$!n', 'does parent lists its attribute';

role R { has $.x; method m { }; submethod BUILD { } }
is R.^methods.map(*.name).sort.join(','), 'BUILD,m', 'role methods: no generated accessor, submethods listed';
is R.^attributes.map(*.name).join(','), '$!x', 'role attributes';

my $c = True but B;
ok $c.^attributes.map(*.name).grep('$!n'), 'mixin of an also-does role lists the parent attribute';

role P { has $.p }
role Q does P { has $.q }
is (5 but Q).^attributes.map(*.name).join(','), '$!q,$!p,$!value',
    'mixin attributes: own before parent before base';
is (5 but Q).^attributes(:local).map(*.name).join(','), '$!q,$!p',
    'mixin attributes :local are the mixed-in roles\'';

class K { has $.k }
is (K.new but R).^attributes.map(*.name).join(','), '$!x,$!k', 'mixin into a class instance';
is K.^attributes.map(*.name).join(','), '$!k', 'class attributes unchanged';
