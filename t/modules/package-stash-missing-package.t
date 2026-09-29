use Test;

# A stash read through a qualified package that does not exist dies the way
# rakudo's does, instead of answering an empty stash (#9845).

plan 14;

throws-like { Foo::Bar::<$quux> }, Exception,
    message => "Could not find symbol '&Bar' in 'GLOBAL::Foo'", 'keyed read, unknown root';
throws-like { Foo::Bar::.keys }, Exception,
    message => "Could not find symbol '&Bar' in 'GLOBAL::Foo'", 'whole stash, unknown root';
throws-like { Q::R::S::<$z> }, Exception,
    message => "Could not find symbol '&S' in 'GLOBAL::Q::R'", 'deeper unknown name';

class A { }
throws-like { A::B::<$x> }, Exception,
    message => "Could not find symbol '&B' in 'A'", 'known root, missing member package';
throws-like { A::B::C::<$x> }, Exception,
    message => "Could not find symbol '&C' in 'A::B'", 'known root, missing nested package';
throws-like { A::B::{'$x'} }, Exception,
    message => "Could not find symbol '&B' in 'A'", 'string-subscript read';
throws-like { A::B::<$x>:exists }, Exception,
    message => "Could not find symbol '&B' in 'A'", 'adverbed read';

module M { our sub f { 'f' } }
throws-like { M::N::<&f> }, Exception,
    message => "Could not find symbol '&N' in 'M'", 'module root';
is M::<&f>(), 'f', 'an existing package still reads its member';

class P::Q { }
lives-ok { P::Q::<$x> }, 'a declared nested class: missing member reads fine';
lives-ok { P::<$x> }, 'its implicit parent package exists';
throws-like { P::R::<$x> }, Exception,
    message => "Could not find symbol '&R' in 'P'", 'sibling of a nested class';

lives-ok { GLOBAL::Nope::<$z> }, 'an explicit GLOBAL:: qualifier does not die';
lives-ok { GLOBAL::<$z> }, 'GLOBAL itself';
