use Test;

# From the Functional::LinkedList distribution: `Cls.ASSIGN-POS(0, $v)` on a
# class's type object runs the class's own `multi method ASSIGN-POS`. The
# built-in vivifying row of `Any` (which every class's MRO reaches) answered
# "Cannot modify an immutable 'Cls' type object" instead.

plan 6;

class LL {
    has $.value;
    multi method ASSIGN-POS(0, $value) { "zero($value)" }
    multi method ASSIGN-POS(::?CLASS:D: UInt $i, $value) { "D($i)" }
}
is LL.ASSIGN-POS(0, 9), 'zero(9)', 'a multi on the type object';
is LL.new.ASSIGN-POS(3, 9), 'D(3)', 'and on an instance';

class MM { multi method foo(0, $v) { "zero($v)" }; multi method foo(::?CLASS:D: UInt $i, $v) { "D" } }
is MM.foo(0, 9), 'zero(9)', 'any multi method, for comparison';

class NN { method ASSIGN-POS($i, $v) { "plain($i,$v)" } }
is NN.ASSIGN-POS(1, 2), 'plain(1,2)', 'a plain method on the type object';

class PP { method push($v) { "mine($v)" }; method AT-POS($i) { "at($i)" } }
is PP.push(4), 'mine(4)', 'a user push on a type object is not the vivifying one';
is PP.AT-POS(2), 'at(2)', 'a user AT-POS';
