use Test;
use MONKEY-SEE-NO-EVAL;

plan 4;

class L {
    method a { EVAL 'my class FA { }; FA.^name' }
    method c { EVAL 'my class FC { method n { ::?CLASS.^name } }; FC.n' }
}

is L.a, 'L::FA', 'a method EVAL names a lexical class in its declaring package';
is L.c, 'L::FC', 'a method of the EVALed class sees the same package';

class Child is L { }
is Child.a, 'L::FA', 'an inherited method keeps its declaring package';
is L.a, 'L::FA', 'a repeated EVAL keeps the declaring package';
