use Test;

# A role that composes another is a narrower constraint than the composed one,
# whichever candidate is declared first.

plan 4;

role SI does Iterator {}
class C does SI { method pull-one { IterationEnd } }
multi sub f(SI $i) { "SI" }
multi sub f(Iterator $i) { "Iterator" }
multi sub g(Iterator $i) { "Iterator" }
multi sub g(SI $i) { "SI" }
is f(C.new), 'SI', 'narrow candidate first';
is g(C.new), 'SI', 'narrow candidate last';

role R2 {}
role R3 does R2 {}
class D does R3 {}
multi sub h(R2 $i) { "R2" }
multi sub h(R3 $i) { "R3" }
multi sub k(R3 $i) { "R3" }
multi sub k(R2 $i) { "R2" }
is h(D.new), 'R3', 'user roles, wide first';
is k(D.new), 'R3', 'user roles, narrow first';
