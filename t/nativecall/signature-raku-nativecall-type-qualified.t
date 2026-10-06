use v6;
use Test;
use NativeCall;

# A NativeCall type is declared in NativeCall::Types, so rakudo spells it with
# that package in a signature's `.raku`/`.gist` and in a parameter's `.raku`.
# The type object's own rendering was already qualified; the signature renderer
# wrote the parameter's type constraint text as the source spelled it.

plan 14;

class Foo { }

sub f(CArray $x) { 1 }
sub g(Pointer:D $x) { 1 }
sub arr(CArray[int32] $z) { 1 }
sub named(CArray :$a, Pointer[void] :$b) { 1 }
sub ret(--> CArray) { CArray }
sub core(Int:D $x, Str $y) { 1 }
sub user(Foo $x, Foo:D $y) { 1 }
sub native-prim(int32 $x) { 1 }

is &f.signature.raku, ':(NativeCall::Types::CArray $x)',
    'a NativeCall class type is qualified in Signature.raku';
is &g.signature.raku, ':(NativeCall::Types::Pointer:D $x)',
    'a definiteness smiley stays after the qualified type';
is &arr.signature.raku, ':(NativeCall::Types::CArray[int32] $z)',
    'a parametrization stays after the qualified type';
is &named.signature.raku,
    ':(NativeCall::Types::CArray :$a, NativeCall::Types::Pointer[NativeCall::Types::void] :$b)',
    'named parameters qualify the type and its type parameter';
is &ret.signature.raku, ':( --> NativeCall::Types::CArray)',
    'a NativeCall return type is qualified';
is &f.signature.gist, '(NativeCall::Types::CArray $x)',
    'Signature.gist qualifies it too';

is &core.signature.raku, ':(Int:D $x, Str $y)', 'core types are left as they are';
is &user.signature.raku, ':(Foo $x, Foo:D $y)', 'user classes are left as they are';
is &native-prim.signature.raku, ':(int32 $x)', 'a native primitive type is left as it is';

is &f.signature.params[0].raku, 'NativeCall::Types::CArray $x',
    'Parameter.raku qualifies the type';
is &g.signature.params[0].raku, 'NativeCall::Types::Pointer:D $x',
    'Parameter.raku keeps the smiley after the qualified type';
is &user.signature.params[1].raku, 'Foo:D $y', 'Parameter.raku leaves a user class alone';

# The parameter's type object (the nominal type, without the smiley) was already
# right and is unaffected.
is &f.signature.params[0].type.raku, 'NativeCall::Types::CArray',
    'the parameter\'s type object is qualified (unchanged)';
is &core.signature.params[0].type.raku, 'Int', 'a core type object is unchanged';
