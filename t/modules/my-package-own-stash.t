use Test;

# Each `my package` declaration is a package of its own with its own stash,
# even when another declaration has the same name (#17477; CompUnit::Util's
# `set-in-WHO` over a fresh `my package tmp` in each EVAL). Checked against
# rakudo.

plan 16;

{ my package P {}; P.WHO<$x> := 1; is P.WHO.keys, '$x', 'a stash entry lands in the package'; }
{ my package P {}; is P.WHO.keys.elems, 0, 'a second declaration of the name starts empty'; }

{ my package Pa {}; Pa.WHO.package_at_key('Foo'); ok Pa.WHO<Foo>:exists, 'a vivified sub-package'; }
{ my package Pa {}; nok Pa.WHO<Foo>:exists, 'is not in another declaration'; }

use MONKEY-SEE-NO-EVAL;
EVAL q|my package tmp {}; BEGIN { tmp.WHO.package_at_key('Foo'); tmp.WHO<Foo>.WHO<$v> := 'first' }|;
is EVAL(q|my package tmp {}; tmp.WHO.keys.elems|), 0, 'a later EVAL declares a fresh package';

my package Body { our $x = 'body'; our sub f { 'f' } }
is $Body::x, 'body', 'an our variable of the body';
is Body::f(), 'f', 'an our sub of the body';
is Body.WHO.keys.sort.join(','), '$x,&f', 'both in the stash';
is Body.^name, 'Body', 'the name is the one the program spelled';

{
    my package Holder { class Inner { method hi { 'hi' } } }
    is Holder::Inner.hi, 'hi', 'a nested type by its qualified name';
    my Holder::Inner $i = Holder::Inner.new;
    is $i.hi, 'hi', 'as a variable type';
}

my module Versioned:ver<1.2.3>:auth<me> { }
is Versioned.^auth, 'me', 'a lexical module keeps its adverbs';

# The same holds for a lexical class, which already had a storage name.
my class VersionedClass:ver<2.0> { class Nested { } }
is VersionedClass.^ver, v2.0, 'a lexical class keeps its version';
my VersionedClass::Nested $n = VersionedClass::Nested.new;
isa-ok $n, VersionedClass::Nested, 'its nested class is a variable type';

my package EnumHolder { our enum Mode <off on> }
is EnumHolder::Mode::on.value, 1, 'an enum value of a lexical package';
is EnumHolder::Mode::on.WHAT.^name, 'Mode', 'its enum keeps its declared name';
