use lib 't/lib';
use Test;

# #9944: an operator candidate imported by one compilation unit (the script, a
# module, a class body) is lexical to that unit. Another module's routines keep
# seeing only the operators they declared or imported themselves.

use OpScope::Where;
use OpScope::Plain;
use OpScope::OwnOp;
use OpScope::ClassBody;
use OpScope::UnitClass;

plan 9;

$OpScope::Where::calls = 0;
is plain-mul(2, 3), 6, 'a module that never imported the operator multiplies with the core *';
is $OpScope::Where::calls, 0, "...without consulting the script's imported where-candidate";

$OpScope::Where::calls = 0;
is own-mul(2, 3), 6, 'a module with its own infix:<*> multiplies Ints with the core *';
is $OpScope::Where::calls, 0, "...and does not see the script's imported candidate either";

$OpScope::Where::calls = 0;
my $x = 2;
is $x * 3, 6, 'the importing script still dispatches through its candidate';
is $OpScope::Where::calls, 1, "...whose where clause runs once in the importing scope";

is class-pow(), 'custom', 'a class-body use reaches that class\'s methods';
is plain-pow(), 8, "...but not the enclosing module's other routines";
is OpScope::UnitClass.m, 'custom', 'a use after `unit class` reaches its methods';
