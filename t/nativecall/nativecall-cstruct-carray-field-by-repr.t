use Test;
use nqp;
use NativeCall;

# #11209 (ADR-11203 §2.4): a `CArray` inside a `CStruct` is selected by
# `is repr('CArray')`, not by the class being called `CArray`. The classes here
# are declared by the program itself, so nothing in them is NativeCall's own
# `CArray`; a field of such a class is one pointer in the struct and reads back
# as that class over the memory it points at, the way MoarVM boxes a CArray
# attribute. Every expected value is rakudo's.

plan 20;

class Ints is repr('CArray') is array_type(int32) { }
my class LexInts is repr('CArray') is array_type(int32) { }

class Holder is repr('CStruct') { has Ints $.arr; has int32 $.n; }
class LexHolder is repr('CStruct') { has LexInts $.arr; has int32 $.n; }

# The same bytes seen as a plain pointer, to put an address into a field the
# way C would: `memcpy` copies it from one struct to the other.
class Raw is repr('CStruct') {
    has Pointer $.p;
    has int32 $.n;
    submethod BUILD(Pointer :$p, :$n) {
        $!p := $p if $p.defined;
        $!n = $n if $n.defined;
    }
}
sub fill(Holder $dst, Raw $src, size_t $n --> Pointer)
    is native('c', v6) is symbol('memcpy') { * }
sub fill-lex(LexHolder $dst, Raw $src, size_t $n --> Pointer)
    is native('c', v6) is symbol('memcpy') { * }

is nativesizeof(Holder), 16, 'a CArray-REPR field is one pointer, then the int32 after padding';
is nativesizeof(LexHolder), 16, 'also when the class is a `my class`';

my $a := nqp::create(Ints);
nqp::bindpos_i($a, 0, 5);
nqp::bindpos_i($a, 1, 8);

my $h := Holder.new;
nok $h.arr.defined, 'an unset field is the type object';
is $h.REPR, 'CStruct', 'the struct is a CStruct';

fill($h, Raw.new(p => nativecast(Pointer, $a), n => 3), nativesizeof(Holder));
is $h.n, 3, 'a field C copied reads back';
ok $h.arr.defined, 'the pointer C stored reads back as an object';
isa-ok $h.arr, Ints, 'of the declared class';
is $h.arr.REPR, 'CArray', 'with the CArray representation';
is-deeply (nqp::atpos_i($h.arr, 0), nqp::atpos_i($h.arr, 1)), (5, 8),
    'whose elements are the C memory the pointer names';
nqp::bindpos_i($h.arr, 1, 9);
is nqp::atpos_i($a, 1), 9, 'and a write through it lands in that memory';

# The same for a class declared in a lexical scope.
my $l := nqp::create(LexInts);
nqp::bindpos_i($l, 0, 6);
nqp::bindpos_i($l, 1, 7);
my $lh := LexHolder.new;
fill-lex($lh, Raw.new(p => nativecast(Pointer, $l), n => 4), nativesizeof(LexHolder));
ok $lh.arr.defined, 'a `my class` field reads back as an object';
is $lh.arr.REPR, 'CArray', 'with the CArray representation';
is-deeply (nqp::atpos_i($lh.arr, 0), nqp::atpos_i($lh.arr, 1)), (6, 7),
    'over the C memory';

# nativecast boxes by REPR too.
my $cast := nativecast(Ints, nativecast(Pointer, $a));
ok $cast.defined, 'nativecast to a CArray-REPR class gives an object';
isa-ok $cast, Ints, 'of that class';
is $cast.REPR, 'CArray', 'with the CArray representation';
is nqp::atpos_i($cast, 0), 5, 'viewing the memory at the address';
nok nativecast(Ints, Pointer).defined, 'a NULL cast is the type object';

# The upstream-style `CArray[T]` in a struct field keeps working beside it.
class Plain is repr('CStruct') {
    has CArray[int32] $.arr;
    has int32 $.n;
    submethod BUILD(:$n, CArray[int32] :$arr) {
        $!n = $n if $n.defined;
        $!arr := $arr if $arr.defined;
    }
}
my $p = Plain.new(n => 1, arr => CArray[int32].new(1, 2, 3));
is-deeply ($p.arr[0], $p.arr[2], $p.n), (1, 3, 1), 'a CArray[int32] field still reads its elements';
is nativesizeof(Plain), 16, 'and is one pointer';
