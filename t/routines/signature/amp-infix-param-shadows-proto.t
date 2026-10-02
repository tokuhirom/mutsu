use Test;

plan 11;

# A `proto sub infix:<word>` alone declares the operator for parsing, and an
# `&infix:<word>` parameter (of a sub or a parameterized role) is the operator
# inside the routine body, shadowing the candidate-less proto (#10516; the
# `BinaryHeap` distribution's `role BinaryHeap[&infix:<precedes>]`).
proto sub infix:<precedes>($, $) {*}

sub f(&infix:<precedes>, $a, $b) { $a precedes $b }
is f(&[<], 1, 2), True,  'builtin &[<] bound to &infix:<precedes> param';
is f(&[>], 1, 2), False, 'builtin &[>] bound to &infix:<precedes> param';
is f(-> $x, $y { "$x|$y" }, 1, 2), '1|2', 'block bound to &infix:<precedes> param';

role Heap[&infix:<precedes>] {
    method t($a, $b) { $a precedes $b }
    method u(\a, \b) { a precedes my \c = b }
}
class C does Heap[&[>]] {}
class D does Heap[&[<]] {}
is C.new.t(1, 2), False, 'role parameter &[>] is the operator in a method';
is D.new.t(1, 2), True,  'role parameter &[<] is the operator in a method';
is D.new.u(1, 2), True,  'operator with a declaration operand on the right';

role Dflt[&infix:<precedes> = * cmp * == Less] {
    method t($a, $b) { $a precedes $b }
}
class Mx does Dflt[* cmp * == More] {}
class Mn does Dflt {}
is Mx.new.t(1, 2), False, 'WhateverCode role argument is the operator';
is Mn.new.t(1, 2), True,  'role parameter default is the operator';

# A builtin operator bound to the parameter compares values, not the
# variables' containers.
sub infix:<@@>($, $) { 42 }
sub g(&infix:<@@>, $a, $b) { $a @@ $b }
is g(&[<], 1, 2), True, 'parameter shadows a package operator of the same name';
is 1 @@ 2, 42, 'package operator still resolves outside';

# The proto alone already declares the operator: this parses, and fails only
# at run time for want of a candidate.
proto sub infix:<nocand>($, $) {*}
dies-ok { 1 nocand 2 }, 'a candidate-less proto operator parses';
