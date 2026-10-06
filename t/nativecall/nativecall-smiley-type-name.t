use v6;
use Test;
use NativeCall;

# A definiteness smiley belongs to the type it follows (#11871): the NativeCall
# builtin types render under their qualified identity
# (`NativeCall::Types::CArray`), and so does `CArray:D` -- the qualification
# used to match only the bare name, so the smiley form kept the imported short
# spelling. Core types and a user's own declaration of a NativeCall name are
# not NativeCall's.
#
# Every expectation below was verified against Rakudo.

plan 15;

is CArray.raku, 'NativeCall::Types::CArray', 'the plain type renders qualified';
is (CArray:D).raku, 'NativeCall::Types::CArray:D', '.raku keeps the :D smiley and qualifies the type';
is (CArray:U).raku, 'NativeCall::Types::CArray:U', '... and the :U smiley';
is (CArray:D).^name, 'NativeCall::Types::CArray:D', '.^name agrees';
is (CArray:D).WHAT.^name, 'NativeCall::Types::CArray:D', '.WHAT.^name agrees';
is NativeCall::Types::CArray:D.^name, 'NativeCall::Types::CArray:D', 'the qualified spelling is unchanged';

is (Pointer:D).raku, 'NativeCall::Types::Pointer:D', 'another NativeCall type under :D';
is (Pointer:U).raku, 'NativeCall::Types::Pointer:U', '... and under :U';
is (size_t:D).^name, 'NativeCall::Types::size_t:D', 'a NativeCall integer alias under :D';

is (int32:D).raku, 'int32:D', 'a native integer type is a core name, not qualified';
is (Int:D).raku, 'Int:D', 'a core type keeps its name under :D';
is (Int:U).^name, 'Int:U', '... and under :U';
is (Str:_).raku, 'Str', 'the :_ smiley is still dropped';

my CArray:D $array = CArray[int32].new;
ok $array ~~ CArray:D, 'the smiley form still matches an instance';
nok CArray ~~ CArray:D, '... and still rejects the type object';
