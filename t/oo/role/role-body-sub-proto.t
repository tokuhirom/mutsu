# A sub-level `proto` in a role body declares one routine for the role: an
# exported one is importable as soon as the role's module loads, and composing
# the role into several classes does not redeclare it. Found via PDF::Class
# (PDF::Filespec's `proto sub to-file(|) is export(:to-file) {*}`).
use Test;
use lib 't/lib';

plan 4;

use RoleBodyUse::Tie :tie-it;

is tie-it('x'), 'tied x', 'exported proto sub from a role module';
is tie-it(3), 'tied int 3', 'its multi candidates dispatch';

role R {
    proto sub f(|) {*}
    multi sub f(Int $x) { "int $x" }
    method m { f(3) }
}
class A does R { }
class B does R { }
is A.new.m, 'int 3', 'role sub proto in the first composing class';
is B.new.m, 'int 3', 'and in a second one';
