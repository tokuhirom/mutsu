use nqp;
use Test;

plan 14;

# `nqp::istype` and `nqp::push_s` / `push_i` / `push_n` have a direct form in
# the VM for plain operands, beside the generic op path. Both forms share one
# implementation; these pin the operand shapes the direct form admits and the
# ones it hands back to the generic path, with the answers rakudo gives.

# istype: a plain value, a container, a type object, Nil, an instance's class.
my $s = 'abc';
ok nqp::istype($s, Str), 'istype on a Scalar holding a Str';
nok nqp::istype($s, Int), 'and against another type';
my @a = 1, 2;
ok nqp::istype(@a[0], Int), 'istype on an element container';
ok nqp::istype(@a, Positional), 'istype against a role';
ok nqp::istype(Nil, Nil), 'Nil as the type operand';
class Point { has $.x }
my $p = Point.new(x => 1);
ok nqp::istype(42, nqp::decont(Int)), 'an Int type object as the operand';
ok nqp::istype($p, $p.WHAT), 'a class taken from an instance';
nok nqp::istype($p, Str), 'an instance against a CORE type';

# push_s / push_i / push_n: each coerces to its kind.
my $l := nqp::list_s();
nqp::push_s($l, 'x');
nqp::push_s($l, 42);
is nqp::join(',', $l), 'x,42', 'push_s stringifies a non-Str';
my str @out;
nqp::push_s(@out, $s) for ^2;
is @out.join('|'), 'abc|abc', 'push_s onto a native str array, from a variable';
is nqp::push_s(@out, 'z'), 'z', 'push_s answers the pushed value';
my $li := nqp::list_i();
nqp::push_i($li, '12');
is nqp::atpos_i($li, 0), 12, 'push_i takes the integer of a Str';
my $ln := nqp::list_n();
nqp::push_n($ln, 2);
is nqp::atpos_n($ln, 0), 2e0, 'push_n takes the Num of an Int';

# A hash is not positional: the REPR check refuses it on either path.
throws-like { nqp::push_s(nqp::hash(), 'x') }, Exception,
    'push_s onto a hash dies';
