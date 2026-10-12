use Test;
use nqp;

# A type object boxes no value: nqp::unbox_* refuses it with MoarVM's
# message instead of answering its string form or 0 (#12911).

plan 12;

class C {}

throws-like { nqp::unbox_s(nqp::bootstr()) }, Exception,
    message => 'Cannot unbox a type object (BOOTStr) to a str.', 'unbox_s of the BOOTStr type object';
throws-like { nqp::unbox_i(nqp::bootint()) }, Exception,
    message => 'Cannot unbox a type object (BOOTInt) to an int.', 'unbox_i of the BOOTInt type object';
throws-like { nqp::unbox_n(nqp::bootnum()) }, Exception,
    message => 'Cannot unbox a type object (BOOTNum) to a num.', 'unbox_n of the BOOTNum type object';
throws-like { nqp::unbox_s(Str) }, Exception,
    message => 'Cannot unbox a type object (Str) to a str.', 'unbox_s of Str';
throws-like { nqp::unbox_i(Int) }, Exception,
    message => 'Cannot unbox a type object (Int) to an int.', 'unbox_i of Int';
throws-like { nqp::unbox_i(C) }, Exception,
    message => 'Cannot unbox a type object (C) to an int.', 'unbox_i of a class';
throws-like { nqp::unbox_i(Bool) }, Exception,
    message => 'Cannot unbox a type object (Bool) to an int.', 'unbox_i of Bool';
throws-like { nqp::unbox_u(Int) }, Exception,
    message => 'Cannot unbox a P6opaque (Int) type object', 'unbox_u of Int';

is nqp::unbox_s('x'), 'x', 'a string still unboxes';
is nqp::unbox_i(5), 5, 'an int still unboxes';
is nqp::unbox_n(1e0), 1e0, 'a num still unboxes';
is nqp::unbox_u(7), 7, 'a uint still unboxes';
