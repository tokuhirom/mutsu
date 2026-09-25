use v6;
use nqp;
use Test;

# nqp::isfalse is the complement of nqp::istrue, using the VM's ordinary
# boolification rules for scalars, type objects, lazy values, and user Bool
# methods.

plan 9;

is nqp::isfalse(0), 1, 'isfalse negates istrue for zero';
is nqp::isfalse(1), 0, 'isfalse negates istrue for a nonzero integer';
is nqp::isfalse(''), 1, 'isfalse negates istrue for an empty string';
is nqp::isfalse('0'), 0, 'isfalse negates istrue for the string "0"';
is nqp::isfalse(Nil), 1, 'isfalse negates istrue for Nil';
is nqp::isfalse(Int), 1, 'isfalse negates istrue for a type object';
is nqp::isfalse((1, 2, 3).grep(* > 5)), 1,
    'isfalse negates istrue for an empty lazy sequence';
is nqp::isfalse((1, 2, 3).grep(* < 5)), 0,
    'isfalse negates istrue for a non-empty lazy sequence';

class False-Bool { method Bool { False } }
is nqp::isfalse(False-Bool.new), 1,
    'isfalse negates istrue for an object with a false Bool method';
