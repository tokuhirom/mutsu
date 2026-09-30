use Test;

# XML::Class t/050: `class B { class B { ... } }` declares B::B, whose methods
# and attributes must not land on the enclosing B.
plan 7;

class B {
    class B {
        has Str $.string;
        method hi { "hi" }
    }
    has B $.bee;
    method outer { "outer" }
}

is B::B.^name, "B::B", "inner name is qualified";
is B::B.hi, "hi", "inner method is on the inner class";
ok !B::B.can("outer"), "enclosing method is not on the inner class";
ok !B.can("hi"), "inner method did not land on the enclosing class";
my $o = B.new(bee => B::B.new(string => "boom"));
is $o.bee.string, "boom", "inner attribute and constructor work";
isa-ok $o.bee, B::B, "attribute typed by the inner class";
is B::B.^attributes.elems, 1, "inner class has its own single attribute";
