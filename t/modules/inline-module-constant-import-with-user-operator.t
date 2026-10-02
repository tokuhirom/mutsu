use Test;

# An exported constant of an inline module, read after `import`, must resolve
# through the import even when a user operator is registered before the
# mainline compiles (which turns off compile-time constant folding). The
# package body's own term slot is restored when the body exits, so reading it
# from the enclosing scope gave Nil (#10558).

plan 3;

module M8 { constant k8 is export = 8; }
import M8;
is k8, 8, 'imported constant reads its value';

module Mp { constant kp = 3; }
is Mp::kp, 3, 'a package constant stays reachable by its package name';

# The operator below is what disables constant folding for the whole unit.
module M3 { our sub infix:<foo>($a, $b) { "$a$b" } }
is M3::infix:<foo>(1, 2), '12', 'the operator itself still works';
