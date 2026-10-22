use lib 't/lib';
use Test;

# EXPORTHOW::SUPERSEDE replaces the HOW of the built-in `class` / `role`
# declarators for the importing unit (AttrX::Mooish's t/lib/CustomHOW, #14241).

plan 6;

use SupersedeBuiltinHOW;
use SupersedeBuiltinProbe;

is (class { }).HOW.^name, 'CustomClassHOW', 'anonymous class uses the superseding HOW';
is (role { }).HOW.^name, 'CustomRoleHOW', 'anonymous role uses the superseding HOW';

class Named { }
is Named.HOW.^name, 'CustomClassHOW', 'named class';
my class Lexical { }
is Lexical.HOW.^name, 'CustomClassHOW', 'my class';
is Named.new.^name, 'Named', 'the class still instantiates';

# The override is lexical to the importing unit: a module that did not import
# it keeps the default HOW.
is supersede-probe-how(), 'Perl6::Metamodel::ClassHOW', 'another unit is unaffected';
