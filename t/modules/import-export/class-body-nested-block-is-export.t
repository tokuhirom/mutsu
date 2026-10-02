use Test;

# An `is export` routine nested in a block of a class body is exported from
# the class at compile time, so `import K` (run at BEGIN time) finds it. The
# BEGIN prologue splits such a class body, and the nested routine lives only
# in the run-time half (#10564).

plan 5;

class K1 { if True { sub g1 is export { 'g1' } } }
class K2 { { sub g2 is export { 'g2' } } }
class K3 { if True { my $x = 4; sub g3 is export { $x } } }
class K4 { sub g4 is export { 'g4' } }
module P { class K5 { if True { sub g5 is export { 'g5' } } } }

import K1;
import K2;
import K3;
import K4;
import P;

is g1(), 'g1', 'sub in an if block of a class body is exported';
is g2(), 'g2', 'sub in a bare block of a class body is exported';
is g3(), 4, 'the exported sub sees the block lexical';
is g4(), 'g4', 'a direct class-body sub still works';
is g5(), 'g5', 'a nested-block sub of a class in a module is exported from the module';
