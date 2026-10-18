use Test;

plan 4;

module Foo::Tree {
    our role Node { method hi { "hi" } }
    our role Leaf does Node { }
}
constant VT = Foo::Tree;

role N does VT::Node { }
class C does N { }
is C.new.hi, 'hi', 'a role can `does` a role named through a constant package alias';

role L does VT::Leaf { }
class D does L { }
is D.new.hi, 'hi', 'also for a role that itself does another';
is VT::Node.^name, 'Foo::Tree::Node', 'the alias resolves at run time';
ok C.new ~~ Foo::Tree::Node, 'and the composed role is the real one';
