use Test;

use lib $*PROGRAM.parent(2).add("roast/packages/AandB/lib");

plan 5;

eval-lives-ok 'use A::A', 'nested unit module loads';
eval-lives-ok 'use A::A; use A::B; A::B::D ~~ A::B::B or die()', 'nested role composition resolves in package scope';
eval-lives-ok 'use A::A; use A::B; A::B::D.new()', 'nested class instantiation works';
eval-lives-ok 'use RoleA', 'unit role declarations parse and load from modules';
eval-lives-ok 'use RoleA; use RoleB; role RoleB {...}; class MyFu does RoleB {}; MyFu ~~ RoleB or die()', 'later non-parametric role shadows imported role';
