use Test;
use NativeCall;

# `CUnion` and `CPPStruct` objects built in Raku own a native body like a
# `CStruct` does (ADR-11209): members overlay at offset 0 in a union, the
# object is passed to C as a real pointer, and both report their own REPR.

plan 22;

class U is repr<CUnion> {
    has int32 $.i is rw;
    has num32 $.f is rw;
    has int8  $.c is rw;
}

my $u = U.new(i => 0x3f800000);
is $u.REPR, 'CUnion', 'a Raku-built CUnion reports CUnion';
is U.REPR, 'CUnion', 'and so does its type object';
is nativesizeof(U), 4, 'a union is as large as its largest member';
is $u.i, 0x3f800000, 'the member that was set';
is $u.f, 1e0, 'a float member sees the same bytes';
is $u.c, 0, 'and a narrower member the low byte';

$u.f = 2e0;
is $u.i, 0x40000000, 'writing one member changes the others';
$u.c = 1;
is $u.i, 0x40000001, 'a narrower write leaves the other bytes alone';

my $z = U.new;
is-deeply ($z.i, $z.f, $z.c), (0, 0e0, 0), 'an empty union is zero';
my $p = nativecast(Pointer, $u);
isnt $p.Int, 0, 'a union has a block C can be handed';
is nativecast(CArray[int32], $p)[0], 0x40000001, 'which holds the bytes';

sub memcpy(U $dst, U $src, size_t $n --> Pointer) is native('c', v6) { * }
my $copy = U.new;
memcpy($copy, $u, nativesizeof(U));
is $copy.i, 0x40000001, 'memcpy between two Raku-built unions copies the bytes';

# The byte-overlay behaviour the old constructor had is unchanged.
class Wide is repr('CUnion') {
    has uint8  $.byte;
    has uint16 $.word;
    has uint32 $.dword;
}
my $w = Wide.new(dword => 0x12345678);
is-deeply ($w.dword, $w.word, $w.byte), (0x12345678, 0x5678, 0x78), 'narrower members read the low bytes';
is Wide.new(word => 0xABCD).dword, 0xABCD, 'a narrower constructor argument fills only its bytes';

# A union as a member of a struct, inline and by pointer.
class Holder is repr<CStruct> {
    has int32 $.tag;
    HAS U $.value;
}
is nativesizeof(Holder), 8, 'a struct with an inline union';
my $h = Holder.new(tag => 5);
$h.value.i = 77;
is-deeply ($h.tag, $h.value.i), (5, 77), 'the inline member is storage in the struct';

# CPPStruct is laid out and allocated as a struct.
class Cpp is repr<CPPStruct> { has int32 $.a; has int32 $.b; }
is Cpp.REPR, 'CPPStruct', 'a CPPStruct type object reports CPPStruct';
my $c = Cpp.new(a => 1, b => 2);
is $c.REPR, 'CPPStruct', 'and so does an instance';
is-deeply ($c.a, $c.b), (1, 2), 'its fields are readable';
is nativesizeof(Cpp), 8, 'and it has a size';
my $cpp = Cpp.new;
sub memcpy-cpp(Cpp $dst, Cpp $src, size_t $n --> Pointer) is native('c', v6) is symbol('memcpy') { * }
memcpy-cpp($cpp, $c, nativesizeof(Cpp));
is-deeply ($cpp.a, $cpp.b), (1, 2), 'and reaches C as a pointer';
is $c.gist, 'Cpp.new(a => 1, b => 2)', 'it prints its fields';
