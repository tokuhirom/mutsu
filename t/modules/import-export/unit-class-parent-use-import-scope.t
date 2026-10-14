use Test;
use lib $?FILE.IO.parent(3).add('fixtures/unit-class-parent-use/lib').Str;

plan 3;

# A `unit class C is P;` (or `also is P`) body's `use` is hoisted ahead of the
# declaration so the module loads before the parents resolve. The import must
# still belong to the class body: when the module is first loaded by a
# `require` inside a parameter default, the scope rollback after that default
# must not take the class's imported multi away (PDF's PDF::COS::Dict).
sub load-dict(:$class = (require ::('UcpuDict'))) { $class }
is load-dict().new.x, 'fa:2', 'unit class with a parent keeps its imported multi';

sub load-role(:$role = (require ::('UcpuRole'))) { $role }
is load-role().r, 'fa:3', 'unit role that does a role keeps its imported multi';

is load-dict().x, 'fa:2', 'reachable from the type object too';
