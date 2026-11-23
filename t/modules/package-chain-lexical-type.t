# A lexical type (`my class`) read as a bareword from a nested class's
# method resolves through the package chain's narrow probes (#18274).
use Test;
module M {
    my class Inner { method tag { 'inner' } }
    class M::K {
        method make { Inner.new }
        method tag { Inner.tag }
    }
}
module N {
    class N::K {
        method tag { my class Inner { method tag { 'local' } }; Inner.tag }
    }
}
for ^3 {
    is M::K.tag, 'inner', "a package block's my class from a nested class's method ($_)";
}
isa-ok M::K.make, M::K.make.WHAT, 'constructs it';
is N::K.tag, 'local', 'a method-local my class';
done-testing;
