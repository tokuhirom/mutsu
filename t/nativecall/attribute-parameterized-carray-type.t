use Test;
use NativeCall;

# `Attribute.type` of `has CArray[uint8] $.a` is the type object the
# declaration denotes, the one `CArray[uint8]` evaluates to (#14672).
# Expected values are the `raku` output.

plan 9;

class S is repr('CStruct') {
    has CArray[uint8] $.a;
    has Pointer $.p;
    has uint32 $.n;
}
my $type = S.^attributes.first(*.name eq '$!a').type;
my $same = CArray[uint8];

ok $type === $same, 'the attribute type is the CArray[uint8] type object';
is $type.^name, 'NativeCall::Types::CArray[uint8]', 'its name';
is $type.allocate(3).elems, 3, '.allocate works on it';
is $type.REPR, 'CArray', 'its REPR';
ok $type.^can('allocate'), 'it can allocate';

is nativesizeof(CArray[uint8]), 8, 'nativesizeof of a parameterized CArray is a pointer';
is nativesizeof($type), 8, 'and of the attribute type';
is nativesizeof(S.^attributes.first(*.name eq '$!p').type), 8, 'a Pointer attribute';
is nativesizeof(S), 24, 'the struct containing them';
