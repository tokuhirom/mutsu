use Test;
plan 3;

proto f($) { {*} + 7 }
multi f(1) { 42 }
is f(1), 49, 'statement-initial {*} followed by an infix is one expression';

proto g($) { {*} ~ "!" }
multi g(Str $s) { $s }
is g("a"), "a!", 'works with ~';

proto h($) { 7 + {*} }
multi h(1) { 42 }
is h(1), 49, '{*} as right operand still works';
