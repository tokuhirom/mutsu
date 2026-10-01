use Test;

plan 5;

class D {
    method foo-bar { "foo-bar" }
    method baz     { "baz" }
    method x1      { "x1" }
}
class A { has $.d handles /<[a..z]>+ '-' bar/ = D.new }
class B { has $.d handles /^ ba z $/ = D.new }
class C { has $.d handles /<alpha> \d/ = D.new }
class E { has $.d handles /^ x \/? 1 $/ = D.new }

is A.new.foo-bar, "foo-bar", 'char class and quoted literal in handles regex';
is B.new.baz, "baz", 'whitespace is insignificant in handles regex';
is C.new.x1, "x1", '<alpha> and \d in handles regex';
is E.new.x1, "x1", 'escaped slash does not end the handles regex';
throws-like { A.new.baz }, X::Method::NotFound, 'non-matching name still fails';
