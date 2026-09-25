use Test;

# A user infix declared at the list-infix level (`is equiv<Z>`, `is
# equiv(&infix:<Z>)`, `is equiv<X>`) is looser than the comma, so the comma
# list around it belongs to its operands (#9405). The built-in `minmax` sits at
# the same level, including with a single neighbouring element, and all of them
# span the comma list of parenthesized call arguments too.

plan 11;

sub infix:<mp> ($t, *@f) is equiv<Z> is assoc<list> { @f.elems }
is (1 mp 2, 3, 4), 3, 'is equiv<Z>: the right operand is the whole comma list';

sub infix:<mq> ($t, *@f) is equiv(&infix:<Z>) { @f.elems }
is (1 mq 2, 3, 4), 3, 'is equiv(&infix:<Z>) does the same';

sub infix:<mj> ($t, *@f) is equiv<Z> { "$t|" ~ @f.join(',') }
my @r = 1 mj 2, 3;
is-deeply @r, ['1|2,3'], 'a list assignment of a list-infix expression';
is (1 mj 2, 3), '1|2,3', 'a single element after the operator';

sub infix:<mx> ($a, $b) is equiv<X> { "[$a|$b]" }
is (1, 2 mx 3, 4), '[1 2|3 4]', 'the elements before the operator join its left operand';
is (1 mx 2), '[1|2]', 'no comma list: two plain operands';

sub f(*@a) { @a.elems }
is f(1 mx 2, 3), 1, 'in parenthesized call arguments too, the comma list is its operand';

sub g(|c) { c.elems }
is g(0, 1 Z 2, 3), 1, 'a built-in Z in call arguments owns the whole comma level';
is g(0, 5 minmax 1, 2), 1, 'so does minmax';

is (5 minmax 3, 2), 2..5, 'minmax with one element after it';
is (1, 5 minmax 3, 2), 1..5, 'minmax with elements on both sides';
