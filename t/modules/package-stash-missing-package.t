use Test;

# A stash read through a qualified package that does not exist reads as an
# empty stash, as under rakudo's RakuAST frontend (the default). The legacy
# frontend died with "Could not find symbol '&Bar' in 'GLOBAL::Foo'" (#9845);
# mutsu followed it until the oracle moved to RakuAST.

plan 17;

is (EVAL 'Foo::Bar::<$quux>'), Any, 'keyed read, unknown root';
is (EVAL 'Foo::Bar::.keys').elems, 0, 'whole stash, unknown root';
is (EVAL 'Q::R::S::<$z>'), Any, 'deeper unknown name';
is (EVAL 'Foo::Bar::{"x"}'), Any, 'string-subscript read';
is (EVAL 'Foo::Bar::<$q>:exists'), False, 'adverbed read';
is (EVAL 'Foo::Bar::.WHAT').^name, 'Stash', 'the stash of a missing package is a Stash';

class A { }
is (EVAL 'A::B::<$x>'), Any, 'known root, missing member package';
is (EVAL 'A::B::C::<$x>'), Any, 'known root, missing nested package';
is (EVAL 'A::B::<$x>:exists'), False, 'adverbed read under a known root';

module M { our sub f { 'f' } }
is (EVAL 'M::N::<&f>'), Any, 'module root';
is M::<&f>(), 'f', 'an existing package still reads its member';

class P::Q { }
lives-ok { P::Q::<$x> }, 'a declared nested class: missing member reads fine';
lives-ok { P::<$x> }, 'its implicit parent package exists';
is (EVAL 'P::R::<$x>'), Any, 'sibling of a nested class';

lives-ok { GLOBAL::Nope::<$z> }, 'an explicit GLOBAL:: qualifier does not die';
lives-ok { GLOBAL::<$z> }, 'GLOBAL itself';

package Real { our $v = 3 }
is Real::<$v>, 3, 'a declared package still answers its variable';
