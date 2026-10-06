use v6;
use Test;
use NativeCall;

# `use NativeCall` imports `CArray`, `Pointer`, `size_t`, ... as aliases for the
# types declared in NativeCall::Types, so a short spelling and its qualified one
# are one type object. mutsu keeps ONE registry key per type, the bare one
# (ADR-0056), and a type object built from the qualified spelling used to carry
# the spelled name instead: the two compared unequal under `===`, `eqv`,
# `.WHICH` and as object-hash keys.

plan 24;

ok CArray === NativeCall::Types::CArray, 'CArray === NativeCall::Types::CArray';
ok NativeCall::Types::CArray === CArray, '... in the other order';
ok (CArray:D) === (NativeCall::Types::CArray:D), 'the :D forms are identical';
ok CArray.WHICH eq NativeCall::Types::CArray.WHICH, 'the two spellings have one .WHICH';
ok Pointer === NativeCall::Types::Pointer, 'Pointer === NativeCall::Types::Pointer';
ok size_t === NativeCall::Types::size_t, 'a lowercase C type is a type, not a sub call';
ok void === NativeCall::Types::void, 'void === NativeCall::Types::void';
ok Pointer[uint8] === NativeCall::Types::Pointer[uint8],
    'a parametrization of the qualified spelling is the short one\'s';
ok CArray[int32] === NativeCall::Types::CArray[int32], 'CArray[int32], both spellings';

ok CArray eqv NativeCall::Types::CArray, 'eqv';
ok CArray =:= NativeCall::Types::CArray, '=:=';
ok CArray ~~ NativeCall::Types::CArray, 'a type object smartmatches its qualified spelling';
ok OpaquePointer === NativeCall::Types::Pointer, 'the OpaquePointer alias is the same type';

my %h{Any};
%h{CArray} = 'short';
is %h{NativeCall::Types::CArray}, 'short', 'one spelling reads what the other stored in an object hash';
is %h.elems, 1, '... and they are one key';

# The qualified spelling is now a working type spelling, not just an equal name.
my $arr = NativeCall::Types::CArray[int32].new(4, 5, 6);
is $arr.elems, 3, 'NativeCall::Types::CArray[int32].new builds a typed array';
sub short(CArray $x) { 'short' }
sub qualified(NativeCall::Types::CArray $x) { 'qualified' }
is short($arr), 'short', 'a qualified-spelling instance binds to the short constraint';
is qualified($arr), 'qualified', 'a short-spelling instance binds to the qualified constraint';

# The qualified spelling still DISPLAYS qualified (ADR-0056).
is NativeCall::Types::CArray.^name, 'NativeCall::Types::CArray', '.^name';
is (NativeCall::Types::Pointer:D).raku, 'NativeCall::Types::Pointer:D', '.raku keeps the smiley';

# Nothing else changes.
ok Int === Int, 'Int === Int';
nok Int === Str, 'Int !=== Str';
class Foo { }
ok Foo === Foo, 'a user class is identical to itself';
{
    my class Scoped { }
    ok Scoped === Scoped, 'a my-scoped class is identical to itself';
}
