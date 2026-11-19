use Test;

# An uncomposed role's attribute belongs to the role's generic `$?CLASS`, a
# GenericHOW type object (AttrX::Mooish tells role attributes apart by it);
# once the role is composed, the class's copy belongs to the class (#18215).

plan 7;

role R { has $.y }
my $p = R.^attributes[0].package;
is $p.HOW.^name, 'Perl6::Metamodel::GenericHOW', 'a role attribute\'s package has a GenericHOW';
ok $p.HOW ~~ Metamodel::GenericHOW, 'it smartmatches Metamodel::GenericHOW';
is $p.^name, '$?CLASS', 'the package is the generic $?CLASS';
nok $p === R, 'not the role itself';

class C does R { has $.z }
is C.^attributes.map(*.package.^name).join(','), 'C,C', 'a composed copy belongs to the class';
nok C.^attributes.map({ .package.HOW ~~ Metamodel::GenericHOW }).any.so, 'and is not generic';

role P[::T] { has T $.v }
is P.^candidates[0].^attributes[0].package.HOW.^name, 'Perl6::Metamodel::GenericHOW',
    'a parametric role candidate\'s attribute too';
