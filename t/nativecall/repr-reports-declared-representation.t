use Test;
use NativeCall;

plan 7;

# `.REPR` must report the representation a class was *declared* with, on the
# type object as well as on a live handle. A NativeCall binding gates on
# exactly that: `NativeHelpers::CStruct`'s `LinearArray[::T]` opens with
# `die "Need a CStruct" unless T.REPR eq 'CStruct'`, so a type object
# answering `P6opaque` killed the role's parameterisation outright.

class AStruct is repr('CStruct') {
    has uint64 $.a;
    has uint64 $.b;
}
class APointer is repr('CPointer') { }
class AUnion is repr('CUnion') {
    has uint64 $.a;
    has uint32 $.b;
}
class Ordinary { has $.x }

is AStruct.REPR, 'CStruct', 'a CStruct type object reports CStruct';
is APointer.REPR, 'CPointer', 'a CPointer type object reports CPointer';
is AUnion.REPR, 'CUnion', 'a CUnion type object reports CUnion';
is Ordinary.REPR, 'P6opaque', 'an ordinary class is still P6opaque';
is Int.REPR, 'P6opaque', 'and so is a built-in type';

sub calloc(size_t, size_t --> Pointer) is native { * }
my $handle = nativecast(AStruct, calloc(1, 32));
is $handle.REPR, 'CStruct', 'a live CStruct handle reports CStruct too';

# The shape that motivated this: a role gating on its type parameter's REPR.
role Guarded[::T] {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    method describe() { "guarded:{T.^name}" }
}
is Guarded[AStruct].describe, 'guarded:AStruct',
    'a role can gate on its type parameter being a CStruct';
# The rejecting half of the same guard is pinned by
# t/role-body-guard-parameterisation.t.
