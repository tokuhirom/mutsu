use Test;
use NativeCall;

# NativeCall's `CArray`, `Pointer` and `size_t` are lexical terms of this unit
# (its `is export` constants bind as pad terms, ADR-8751). A top-level routine
# that names one captures it, which boxes the term into a shared cell. Every
# place that reads such a name as a type must see through that cell: a later
# routine's signature, a CStruct field, a return type check.

plan 8;

sub captures-types($w) { (CArray, size_t, Pointer.new($w)) }

sub take-array(CArray[int32], size_t) { }
is &take-array.signature.params[0].type.^name, 'NativeCall::Types::CArray[int32]',
    'a parameterized parameter type names the aliased type';
is &take-array.signature.params[1].type.^name, 'NativeCall::Types::size_t',
    'a plain alias in a later signature names the aliased type';

sub ptr-of(size_t $x --> Pointer) { Pointer.new($x) }
is &ptr-of.signature.returns.^name, 'NativeCall::Types::Pointer',
    'the return type names the aliased type';
is ptr-of(5).Int, 5, 'and the return value passes its type check';

class S is repr('CStruct') {
    has size_t  $.n;
    has Pointer $.p;
}
is nativesizeof(S), 16, 'the struct lays out both fields';
my $c = CArray[uint64].new(1234, 7);
my $s = nativecast(S, nativecast(Pointer, $c));
is $s.n, 1234, 'an aliased integer field reads back';
isa-ok $s.p, Pointer, 'a Pointer field reads back as a Pointer';
is $s.p.Int, 7, 'holding the stored address';
