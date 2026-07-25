use v6;
use Test;
use NativeCall;

plan 15;

# `nativesizeof` reports how many bytes a type occupies in C.
is nativesizeof(int8),   1, 'nativesizeof(int8)';
is nativesizeof(uint8),  1, 'nativesizeof(uint8)';
is nativesizeof(uint32), 4, 'nativesizeof(uint32)';
is nativesizeof(num32),  4, 'nativesizeof(num32)';
is nativesizeof(uint64), 8, 'nativesizeof(uint64)';
is nativesizeof(num64),  8, 'nativesizeof(num64)';
is nativesizeof(Pointer), 8, 'nativesizeof(Pointer) is one pointer';

# A CStruct reports its own padded size, not one pointer.
class Padded is repr('CStruct') {
    has int8  $.a;
    has int32 $.b;
    has int8  $.c;
}
is nativesizeof(Padded), 12, 'a CStruct is padded to its strictest member';

class Ptrish is repr('CStruct') {
    has Pointer $.p;
    has int32   $.n;
}
is nativesizeof(Ptrish), 16, 'a CStruct with a pointer member rounds up to 8';

# `Pointer.new` takes the address positionally, as Rakudo does.
is Pointer.new(0xdeadbeaf).Int, 0xdeadbeaf, 'Pointer.new($address)';
is Pointer.new.Int, 0, 'Pointer.new defaults to the null address';
ok Pointer.new.defined, 'a null Pointer built from Raku is still defined';

# `.WHERE` on a Pointer is a real address whose first machine word holds the
# pointer value. `NativeHelpers`' MoarVM::Guts::REPRs derives the offset of an
# object's payload this way, so handing it an identity hash would make it read
# wild memory.
my Pointer \p = Pointer.new(0xdeadbeaf);
ok p.WHERE != 0, '.WHERE on a Pointer is a non-zero address';

# The declared constraint is written with a `constant` type alias, exactly as
# MoarVM::Guts::REPRs writes it; it has to match the resolved spelling the value
# carries.
constant intptr = uint64;
my CArray[intptr] \ar = nativecast(CArray[intptr], Pointer.new(p.WHERE));

# How far into the object the payload sits is implementation-defined (Rakudo
# puts it past MoarVM's object header; mutsu's `.WHERE` points straight at it),
# so scan for it the way the module does rather than assuming an offset.
my $i = 0;
repeat { last if ar[$i] == p.Int; } while ++$i < 10;
ok $i < 10, 'the payload is found within the first ten words';
is ar[$i], 0xdeadbeaf, 'the word found by the scan is the pointer value';
