use Test;

plan 3;

# A subset-typed (refinement) positional makes a candidate narrower; a named
# parameter's constraint decides applicability only, never narrowness.
class G {
    multi method g(UInt:D $size = 1) { "pos" }
    multi method g(UInt:D :$size = 1) { "named" }
}
is G.new.g, "pos", 'UInt:D optional positional beats UInt:D named-only';

class A {
    multi method g(UInt $s = 1) { "pos" }
    multi method g(UInt :$s = 1) { "named" }
}
is A.new.g, "pos", 'UInt optional positional beats UInt named-only';

class D {
    multi method g(UInt $s = 1) { "pos" }
    multi method g(:$s = 1) { "named" }
}
is D.new.g, "pos", 'subset positional beats untyped named-only';
