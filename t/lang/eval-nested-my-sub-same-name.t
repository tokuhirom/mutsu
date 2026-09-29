use Test;

# Found via Services::PortMapping's t/00-load.t: `use-ok` EVALs a `use` whose
# module chain runs Slangify's `sub EXPORT`, which declares a nested
# `my sub EXPORT`. Inside an EVAL that nested declaration was rejected as a
# redeclaration of the enclosing routine.

plan 4;

sub outer() {
    my sub outer() { 42 }
    outer()
}
is outer(), 42, 'nested my sub shadows its enclosing sub outside EVAL';

is EVAL(q[sub outer2() { my sub outer2() { 7 }; outer2() }; outer2()]), 7,
    'nested my sub shadows its enclosing sub inside EVAL';

is EVAL(q[sub outer3($x) { my sub outer3() { $x }; Map.new(('&outer3' => &outer3,)) }; outer3(8)<&outer3>()]), 8,
    'the nested sub is exportable as a value from an EVAL-declared sub';

throws-like { EVAL q[sub dup() { 1 }; sub dup() { 2 }] }, X::Redeclaration,
    'a genuine top-level redeclaration inside EVAL is still rejected';

done-testing;
